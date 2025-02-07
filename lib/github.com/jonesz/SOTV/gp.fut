import "kernel"
import "mean"
import "../../athas/vector/vector"
import "../../diku-dk/linalg/linalg"
import "../../diku-dk/linalg/lu"

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
