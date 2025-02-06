module type policy = {
	type v
	type s

	val f [n] : [n]v -> [n]s -> v -> s
}
