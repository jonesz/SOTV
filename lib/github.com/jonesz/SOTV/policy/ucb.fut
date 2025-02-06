import "../gp"
import "policy"

module mk_ucb (R: real) (GP: gp with s = R.t) (P: { val kappa: R.t }): policy = {
	type v = GP.v
	type s = R.t

	def f x f x_next = 
		let (u, sigma) = GP.predictive_dist x f x_next
		in (R.sqrt) sigma |> (R.*) P.kappa |> (R.+) u
}
