import "../../athas/vector/vector"

module type mean = {
	--| A row vector.
	type v
	--| A scalar type.
	type s

	val u : v -> s
}

module mk_mean_zero (R: real) (V: vector) : mean with s = R.t = {
	type v = V.vector R.t
	type s = R.t

	def u _ = (R.i64 0) 
}
