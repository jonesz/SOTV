import "kernel"
import "mean"
import "../../diku-dk/linalg/linalg"

module type gp = {
	type v
	type s

	val predictive_dist [n] : [n]v -> [n]s -> v -> (s, s)
}

module mk_gp (R: real) (U: mean with s = R.t) (A: kernel with s = R.t with v = U.v) : gp = {
	type v = U.v
	type s = R.t

	-- We need this for an inverse operation; it's Guass Jordan unfortunately.
	-- TODO: There's QR, LU elsewhere.
	module L = mk_ordered_linalg R

	-- TODO: Rename, this is confusing with `k`.
 	def K [n] (x: [n]v) =
		-- TODO: This is symmetric and can be minimized.
 		map (\x_i -> map (\x_j-> A.kernel x_i x_j) x) x
 
	-- TODO: Rename, this is confusing with 'K'.
 	def k [n] (x: [n]v) x_next = 
 		map (A.kernel x_next) x
 
 	def predictive_dist [n] (x: [n]v) (f: [n]s) x_next =
 		let K_inv = K x |> L.inv
 		let k_tmp = k x x_next
 		let u_t =
 			map (U.u) x
 			|> map2 (R.-) f
 			|> L.matvecmul_row K_inv
 			|> L.dotprod k_tmp
 
 		let stdev_t = L.matvecmul_row K_inv k_tmp |> L.dotprod k_tmp
 					|> (R.-) (A.kernel x_next x_next)
 
 		in (u_t, stdev_t)
}
