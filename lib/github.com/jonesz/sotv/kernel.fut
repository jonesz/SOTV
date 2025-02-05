import "../../athas/vector/vector"
import "../../athas/vector/vspace"

module type kernel = {
	--| A row vector.
	type v
	--| A scalar type.
	type s

	--| The parameters for the kernel.
	type p

	val k : p -> v -> v -> s
}

--| Squared exponential kernel.
module mk_kernel_se(T: vector) (R: real): kernel = {
	-- TODO: I would rather pass in the vpsace itself than create it within the module, but
	-- I'm unable to get the type checker to work...
	module V = mk_vspace T R

	type v = V.vector
	type s = R.t
	
	type p = {l: s, sigma: s}

	def k (s: {l: r, sigma: s}) a b =
		let d: s = (R.**) p.l (R.i64 2) |> (R.*) (R.i64 2)
		in (V.-) a b |> V.norm |> flip (R./) d |> R.neg |> R.exp |> (R.*) p.sigma |> (R.*) p.sigma
}
