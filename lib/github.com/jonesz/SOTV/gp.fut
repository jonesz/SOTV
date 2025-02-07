import "kernel"
import "mean"
import "../../athas/vector/vector"
import "../../diku-dk/linalg/linalg"

-- For the notes, theorem numbers, etc., see "Bayesian Optimization" by Roman Garnett.

module type gp = {
	--| A row vector.
	type v

	--| A scalar type.
	type s

	type mu = s
	type stddev = s

	val compute_C_inv [n] : [n]v -> [n][n]s

	--| Given a set of observations and a potential new point, return `(mu, stddev)`.
	val predictive_dist [n] : [n][n]s -> [n](v, s) -> v -> (mu, stddev)
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
 
	-- (2.8)
 	def crosscov x x_next = 
 		map (K.kernel x_next) x

	-- (2.3) ; the computation of the inverse of the Gram matrix.
	def compute_C_inv x = 
		-- TODO: This is symmetric and thus can be stored in a triangular matrix.
 		map (\x_i -> map (\x_j-> K.kernel x_i x_j) x) x |> L.inv
 
	-- (2.10)
 	def predictive_dist C_inv obs x_next =
		let (x, f) = unzip obs
 		let kx = crosscov x x_next
 		let u_t =
 			map (U.u) x                   -- m
 			|> map2 (R.-) f               -- (y - m)
 			|> L.matvecmul_row C_inv      -- C_inv (y - m)
 			|> L.dotprod kx               -- k(x)^T C_inv (y - m)
			|> (R.+) (U.u x_next)         -- u(x) + k(x)^T C_inv (y - m)
 
 		let stdev_t =
			L.matvecmul_row C_inv kx          -- C_inv kx
			|> L.dotprod kx                   -- kx^T C_inv kx
 			|> (R.-) (K.kernel x_next x_next) -- k(x', x') kx^T C_inv kx

  		in (u_t, stdev_t)
}
