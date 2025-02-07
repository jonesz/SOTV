import "policy/policy"
import "policy/opt/gd"

module mk_sotv (R: real) (P: policy with v [n] = [n]R.t with s = R.t) (GD: optimizer with v [n] = [n]R.t with s = R.t) = {
	type v [n] = [n]R.t
	type s = R.t

	def next [n] bb_fn (k: i64) (x_0: v[n]) =
		let D_x : [k]v[n] = replicate k (replicate n (R.i64 0))
		let D_f : [k]R.t  = replicate k (R.i64 0)
		let D_x = D_x with [0] = x_0
		let D_f = D_f with [0] = bb_fn x_0

	 	in loop (i, D_x, D_f) = (1i64, D_x, D_f) while i < k do
			let x_next = GD.opt (P.f (take i D_x) (take i D_f)) (take i D_x |> last) 100i64
			let f_next = bb_fn x_next

			-- TODO: These copies are *awful* -- see if we can get rid of them.
			in (i + 1i64, copy D_x with [i] = x_next, copy D_f with [i] = f_next)
}
