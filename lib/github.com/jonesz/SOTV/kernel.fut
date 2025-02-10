import "../../athas/vector/vector"
import "../../athas/vector/vspace"

module type kernel = {
  --| A row vector.
  type v

  --| A scalar type.
  type s

  --| k(x_0, x_1).
  val kernel : v -> v -> s
}

--| Squared Exponential Kernel.
module mk_kernel_se
  (R: real)
  (V: vector)
  (S: vspace with vector = V.vector R.t with real = R.t)
  (P: {
    val l : R.t
    val sigma : R.t
  })
  : kernel with v = V.vector R.t with s = R.t = {
  type v = V.vector R.t
  type s = R.t

  def kernel x_0 x_1 =
    let z = (S.-) x_0 x_1
    in S.scale ((R.**) P.l (R.i64 2) |> (R.recip)) z
    |> S.dot z
    |> (R.*) (R.i64 2 |> R.recip)
    |> R.neg
    |> R.exp
    |> (R.*) ((R.**) P.sigma (R.i64 2))

    in map ((R.*) (kernel x_0 x_1)) rhs
}

--| Rational Quadratic Kernel.
module mk_kernel_rq
  (R: real)
  (V: vector)
  (S: vspace with vector = V.vector R.t with real = R.t)
  (P: {
    val l : R.t
    val sigma : R.t
    val a : R.t
  })
  : kernel with v = V.vector R.t with s = R.t = {
  type v = V.vector R.t
  type s = R.t

  def kernel x x' =
    -- (sigma^2)*(1+(||x-x'||^2)/(2al^2))^(-a)
    (S.-) x x' |> S.norm |> flip (R./) ((R.**) P.l (R.i64 2) |> (R.*) P.a |> (R.*) (R.i64 2))
    |> (R.+) (R.i64 1)
    |> flip (R.**) (R.neg P.a)
    |> (R.*) ((R.**) P.sigma (R.i64 2))
}
