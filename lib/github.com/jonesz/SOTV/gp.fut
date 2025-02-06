import "kernel"
import "mean"
import "../../athas/vector/vector"
import "../../diku-dk/linalg/linalg"

module type gp = {
	type v [n]
	type s

	val predictive_dist 'a [m][n] : [m]v[n] -> [m]s -> v[n] -> (s, s)
}

module mk_gp (R: real) (U: mean with v [n] = [n]R.t with s = R.t) (A: kernel with v[n] = [n]R.t with s = R.t) : gp with v [n] = [n]R.t = {
	type v [n] = [n]R.t
	type s = R.t

	-- We need this for an inverse operation; it's Guass Jordan unfortunately.
	-- TODO: There's QR, LU elsewhere.
	module L = mk_ordered_linalg R

	-- TODO: Rename, this is confusing with `k`.
 	def K x =
		-- TODO: This is symmetric and can be minimized.
 		map (\x_i -> map (\x_j-> A.kernel x_i x_j) x) x
 
	-- TODO: Rename, this is confusing with 'K'.
 	def k x x_next = 
 		map (A.kernel x_next) x
 
	-- TODO: This is noiseless.
 	def predictive_dist x f x_next =
 		let K_inv = K x |> L.inv
 		let k_tmp = k x x_next
 		let u_t =
 			map (U.u) x
 			|> map2 (R.-) f
 			|> L.matvecmul_row K_inv
 			|> L.dotprod k_tmp
			|> (R.+) (U.u x_next)
 
 		let stdev_t = L.matvecmul_row K_inv k_tmp |> L.dotprod k_tmp
 					|> (R.-) (A.kernel x_next x_next)
 
 		in (u_t, stdev_t)
}
