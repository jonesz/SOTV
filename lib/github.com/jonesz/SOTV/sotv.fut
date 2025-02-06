import "policy/policy"

module mk_sotv (R: real) (P: policy) = {
	type v = P.v
	type s = R.t

	def next bb_fn (k: i64) initial =
		let D = [(initial, bb_fn initial)]
		in loop D = D for i < k do
			let (x, f) = unzip D
			-- let x_next = P.f x f initial
			let x_next = ???
			let y_next = bb_fn x_next
			in D ++ [(x_next, y_next)]
}
