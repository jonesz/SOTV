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

--| Squared exponential kernel.
module mk_kernel_se(R: real) (V: vspace with real = R.t): kernel = {
	type v = V.vector
	type s = R.t

	type p = {l: s, sigma: s}

	-- sigma^2 e^(-(||x - x'||^2)/(2l^2))
	def K (p: {l: s, sigma: s}) x x' =
		let d = (R.**) p.l (R.i64 2) |> (R.*) (R.i64 2)
		in (V.-) x x' |> V.norm |> flip (R./) d |> R.neg |> R.exp |> (R.*) ((R.**) p.sigma (R.i64 2))
}
