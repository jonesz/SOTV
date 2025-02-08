import "kernel"
import "mean"
import "../../athas/vector/vector"
import "../../diku-dk/linalg/linalg"
import "../../diku-dk/linalg/lu"
import "../../jonesz/krylov-fut/cgm"

-- For the notes, theorem numbers, etc., see "Bayesian Optimization" by Roman Garnett.

module type gp = {
	--| A row vector.
	type v
	--| A scalar type.
	type s

	type mu = s
	type stddev = s

	val compute_u_inv [n] : [n]v -> [n]s -> [n]s
	-- val compute_C_inv [n] : [n]v -> [n][n]s

	--| Given a set of observations and a potential new point, return `(mu, stddev)`.
	val predictive_dist [n] : [n]s -> [n](v, s) -> v -> (mu, stddev)
}

module mk_direct_gp
	(R: real)
	(V: vector)
	(U: mean with v = V.vector R.t with s = R.t)
	(K: kernel with v = V.vector R.t with s = R.t)
	: gp with v = V.vector R.t with s = R.t = {

	type v = V.vector R.t
	type s = R.t
	type mu = s
	type stddev = s

	-- We need this for an inverse operation; it's Guass Jordan unfortunately.
	-- TODO: There's QR, LU elsewhere.
	module L = mk_ordered_linalg R
	module LU = mk_lu R
 
	-- (2.8)
 	def crosscov x x_next = 
 		map (K.kernel x_next) x

	-- (2.3) ; the computation of the inverse of the Gram matrix.
	def compute_u_inv x f = 
		-- TODO: This is symmetric and thus can be stored in a triangular matrix.
		-- Ka = (Y - m(X))
		let b = map (U.u) x |> map2 (R.-) f
		in map (\x_i -> map (\x_j-> K.kernel x_i x_j) x) x |> flip (LU.ols 16) b
		 
	-- (2.10)
 	def predictive_dist u obs x_next =
		let (x, f) = unzip obs
 		let kx = crosscov x x_next
 		let u_t =
 			L.dotprod u kx                -- k(x)^T C_inv (y - m)
			|> (R.+) (U.u x_next)         -- u(x) + k(x)^T C_inv (y - m)
 
 		let stdev_t =
			let b = map (U.u) x |> map2 (R.-) f
			in map (\x_i -> map (\x_j-> K.kernel x_i x_j) x) x |> flip (LU.ols 16) b
			|> L.dotprod kx                   -- kx^T C_inv kx
 			|> (R.-) (K.kernel x_next x_next) -- k(x', x') kx^T C_inv kx

  		in (u_t, stdev_t)
}

module mk_cg_gp
	(R: real)
	(V: vector)
	(U: mean with v = V.vector R.t with s = R.t)
	(K: kernel with v = V.vector R.t with s = R.t)
	: gp with v = V.vector R.t with s = R.t = {

	type v = V.vector R.t
	type s = R.t
	type mu = s
	type stddev = s

	module S = mk_cgm R
	module L = mk_linalg R
 
	-- (2.8)
 	def crosscov x x_next = 
 		map (K.kernel x_next) x

	-- (2.3) ; the computation of the inverse of the Gram matrix.
	def compute_u_inv [n] x f = 
		-- TODO: This is symmetric and thus can be stored in a triangular matrix.
		-- Ka = (Y - m(X))
		let b = map (U.u) x |> map2 (R.-) f
		let A = map (\x_i -> map (\x_j-> K.kernel x_i x_j) x) x
		in S.cgm A b (replicate n (R.i64 0)) (R.f32 0.001f32) 1000i64

	def compute_sigma_inv [n] x kx =
		let b = kx
		let A = map (\x_i -> map (\x_j -> K.kernel x_i x_j) x) x
		in S.cgm A b (replicate n (R.i64 0)) (R.f32 0.001f32) 1000i64 
		 
	-- (2.10)
 	def predictive_dist [n] u sigma obs x_next =
		let (x, _) = unzip obs
 		let kx = crosscov x x_next
		-- u_t = u(x_next) + k^T K^(-1) (f - m)
 		let u_t =
 			L.dotprod u kx                -- k(x)^T C_inv (y - m)
			|> (R.+) (U.u x_next)         -- u(x) + k(x)^T C_inv (y - m)
 
 		let stdev_t =
			L.dotprod sigma kx                   -- kx^T C_inv kx
 			|> (R.-) (K.kernel x_next x_next)    -- k(x', x') kx^T C_inv kx

  		in (u_t, stdev_t)
}

module mk_gp_cg_sym
	(R: real)
	(V: vector)
	(U: mean with v = V.vector R.t with s = R.t)
	(K: kernel with v = V.vector R.t with s = R.t)
	(S: symmetric_matrix with t = R.t)
	(C: cgm with t = R.t) : gp = {

		def solve_mu_inv =
			let b = map (U.u) x |> map2 (R.-) f
			C.cgm A b 
		def solve_sigma_inv = 

		def predictive_dist [n] mu_inv sigma_inv =

		
	}
