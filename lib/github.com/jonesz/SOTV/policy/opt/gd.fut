module type optimizer = {
	type v [n]
	type s

	val opt [n] : (v[n] -> s) -> v[n] -> i64 -> v[n]
}

module mk_gd (R: real) : optimizer = {
	type v [n] = [n]R.t
	type s = R.t

	def grad f x = vjp f x (R.i64 1) -- TODO: I have no idea what this last value means

	def opt f  x_0 (max_iter: i64) =
		let step = ???
		in loop x = x_0 for _i < max_iter do
			grad f x |> map (R.* step) |> map2 (R.-) x
}

