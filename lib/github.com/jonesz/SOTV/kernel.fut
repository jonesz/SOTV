import "../../athas/vector/vector"
import "../../athas/vector/vspace"

-- | Kernel functions.
module type kernel = {
  -- | A row vector.
  type v

  -- | A scalar type.
  type s

  -- | Compute the kernel funciton `k(x_0, x_1)`.
  val kernel : v -> v -> s
}

--| The Squared Exponential Kernel/Radial Basis Function kernel.
module mk_kernel_se
  (R: real)
  (V: vector)
  (P: {
    val l : R.t
    val sigma : R.t
  })
  : kernel with v = V.vector R.t with s = R.t = {
  type v = V.vector R.t
  type s = R.t
  module S = mk_vspace V R

  def kernel x_0 x_1 =
    (S.-) x_0 x_1 |> S.quadrance         -- x := ||x_0 - x_1||^2
    |> (R.*) ((R.**) P.l (R.i64 2)
              |> (R.*) (R.i64 2)
              |> R.recip
              |> R.neg)                  -- -x/(2l^2)
    |> R.exp                             -- exp(-x/(2l^2))
    |> (R.*) ((R.**) P.sigma (R.i64 2))  -- sigma^2 * exp(-x/(2l^2))
}

--| Rational Quadratic Kernel.
module mk_kernel_rq
  (R: real)
  (V: vector)
  (P: {
    val l : R.t
    val sigma : R.t
    val a : R.t
  })
  : kernel with v = V.vector R.t with s = R.t = {
  type v = V.vector R.t
  type s = R.t
  module S = mk_vspace V R

  def kernel x_0 x_1 =
    (S.-) x_0 x_1 |> S.quadrance        -- x := ||x_0 - x_1||^2
    |> (R.*) ((R.**) P.l (R.i64 2)
              |> (R.*) (R.i64 2)
              |> (R.*) P.a
              |> R.recip)               -- x/(2al^2)
    |> (R.+) (R.i64 1)                  -- 1 + x/(2al^2)
    |> flip (R.**) (R.neg P.a)          -- (1 + x/(2al^2))^(-a)
    |> (R.*) ((R.**) P.sigma (R.i64 2)) -- sigma^2 * (1+x/(2al^2))^(-a)
}

module add_kernel
  (R: real)
  (V: vector)
  (A: kernel with s = R.t with v = V.vector R.t)
  (B: kernel with s = R.t with v = V.vector R.t)
  : kernel with s = R.t with v = V.vector R.t = {
  type v = V.vector R.t
  type s = R.t

  def kernel x_0 x_1 =
    (R.+) (A.kernel x_0 x_1) (B.kernel x_0 x_1)
}

module mul_kernel
  (R: real)
  (V: vector)
  (A: kernel with s = R.t with v = V.vector R.t)
  (B: kernel with s = R.t with v = V.vector R.t)
  : kernel with s = R.t with v = V.vector R.t = {
  type v = V.vector R.t
  type s = R.t

  def kernel x_0 x_1 =
    (R.*) (A.kernel x_0 x_1) (B.kernel x_0 x_1)
}
