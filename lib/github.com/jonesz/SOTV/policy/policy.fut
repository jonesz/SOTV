module type policy = {
	type v [n]
	type s

	val f [m][n] : [m]v[n] -> [n]s -> v[n] -> s
}
