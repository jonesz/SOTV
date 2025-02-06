import "../../athas/vector/vector"
import "../../athas/vector/vspace"

module type kernel = {
  --| A row vector.
  type v 

  --| A scalar type.
  type s

  val kernel : v -> v -> s
}

--| Squared Exponential Kernel.
module mk_kernel_se (R: real) (V: vspace with real = R.t) (P: { val l: R.t
                                                                val sigma: R.t }):
                    kernel with v = V.vector with s = R.t = {
  type v = V.vector
  type s = R.t

  -- sigma^2 e^(-(||x - x'||^2)/(2l^2))
  def kernel x x' =
    let d = (R.**) P.l (R.i64 2) |> (R.*) (R.i64 2)
    in (V.-) x x' |> V.norm |> flip (R./) d |> R.neg |> R.exp |> (R.*) ((R.**) P.sigma (R.i64 2))
}

module mk_kernel_se_arr (R: real) (P: { val l: R.t
                                        val sigma: R.t
                                        val dim: i64}):
                    kernel with v = [P.dim]R.t with s = R.t = {
  type v = [P.dim]R.t
  type s = R.t

  def vecsub a b = map2 (R.-) a b
  def vecnorm a = reduce (R.+) (R.i64 0) a |> R.sqrt

  -- sigma^2 e^(-(||x - x'||^2)/(2l^2))
  def kernel x x' =
    let d = (R.**) P.l (R.i64 2) |> (R.*) (R.i64 2)
    in vecsub x x' |> vecnorm |> flip (R./) d |> R.neg |> R.exp |> (R.*) ((R.**) P.sigma (R.i64 2))
}

--| Rational Quadratic Kernel.
module mk_kernel_rq (R: real) (V: vspace with real = R.t) (P: { val l: R.t
                                                                val sigma: R.t
                                                                val a: R.t}):
                    kernel with v = V.vector with s = R.t = {
  type v = V.vector
  type s = R.t

  -- sigma^2 (1 + (||x - x'||^2)/(2al^2))^(-a)
  def kernel x x' =
    let d = (R.**) P.l (R.i64 2) |> (R.*) P.a |> (R.*) (R.i64 2)
    in (V.-) x x' |> V.norm |> flip (R./) d |> (R.+) (R.i64 1) |> flip (R.**) (R.neg P.a) |> (R.*) ((R.**) P.sigma (R.i64 2))
}
