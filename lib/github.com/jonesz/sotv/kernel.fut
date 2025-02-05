import "../../athas/vector/vector"
import "../../athas/vector/vspace"

module type kernel = {
	--| The underlying real type of the vector.
	type r
	--| Some vector type.
	type t

	--| The parameters for the kernel.
	type p

	val k : p -> t -> t -> r
}

--| Squared exponential kernel.
module mk_kernel_se(T: vector) (R: real): kernel = {
	-- TODO: I would rather pass in the vpsace itself than create it within the module, but
	-- I'm unable to get the type checker to work...
	module V = mk_vspace T R

	type t = V.vector
	type r = R.t
	
	type p = {l: r, sigma: r}

	def k (p: {l: r, sigma: r}) a b =
		let d: r = (R.**) p.l (R.i64 2) |> (R.*) (R.i64 2)
		in (V.-) a b |> V.norm |> flip (R./) d |> R.neg |> R.exp |> (R.*) p.sigma |> (R.*) p.sigma
}
