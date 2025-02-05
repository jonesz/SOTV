import "../../athas/vector/vector"
import "../../athas/vector/vspace"

module type kernel = {
	--| A row vector.
	type v
	--| A scalar type.
	type s
	--| The parameters for the kernel.
	type p

	val K : p -> v -> v -> s
}

--| Squared Exponential Kernel.
module mk_kernel_se(R: real) (V: vspace with real = R.t): kernel = {
	type v = V.vector
	type s = R.t

	type p = {l: s, sigma: s}

	-- sigma^2 e^(-(||x - x'||^2)/(2l^2))
	def K (p: {l: s, sigma: s}) x x' =
		let d = (R.**) p.l (R.i64 2) |> (R.*) (R.i64 2)
		in (V.-) x x' |> V.norm |> flip (R./) d |> R.neg |> R.exp |> (R.*) ((R.**) p.sigma (R.i64 2))
}

--| Rational Quadratic Kernel.
module mk_kernel_rq(R: real) (V: vspace with real = R.t): kernel = {
	type v = V.vector
	type s = R.t

	type p = {l: s, sigma: s, a: s}

	-- sigma^2 (1 + (||x - x'||^2)/(2al^2))^(-a)
	def K (p: {l: s, sigma: s, a: s}) x x' =
		let d = (R.**) p.l (R.i64 2) |> (R.*) p.a |> (R.*) (R.i64 2)
		in (V.-) x x' |> V.norm |> flip (R./) d |> (R.+) (R.i64 1) |> flip (R.**) (R.neg p.a) |> (R.*) ((R.**) p.sigma (R.i64 2))
}
