import "../../athas/vector/vector"

module type mean = {
  --| A row vector.
  type v

  --| A scalar type.
  type s

  val u : v -> s
}

module mk_mean_constant (R: real) (V: vector) (P: {val constant : R.t})
  : mean with v = V.vector R.t with s = R.t = {
  type v = V.vector R.t
  type s = R.t

  def u _ = P.constant
}
