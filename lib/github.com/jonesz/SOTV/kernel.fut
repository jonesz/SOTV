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
    -- | A length scale matrix where the diagonal is `l_i^-2`, represented
    -- as a single vector.
    val l : V.vector R.t
    val sigma : R.t
  })
  : kernel with v = V.vector R.t with s = R.t = {
  type v = V.vector R.t
  type s = R.t
  module S = mk_vspace V R

  def kernel x_0 x_1 =
    S.map (\x_i -> (R.**) x_i (R.i64 2) |> R.recip) P.l  -- L^-2
    |> (S.*) ((S.-) x_0 x_1)                             -- L^-2 * (x_0 - x_1)
    |> S.dot ((S.-) x_0 x_1)                             -- x := (x_0 - x_1)^T * L^-2 * (x_0 - x_1)
    |> (R.*) (R.i64 2 |> R.recip)                        -- 1/2 * x
    |> R.neg                                             -- 1/2 * x * -1
    |> R.exp                                             -- exp(1/2 * x * -1)
    |> (R.*) ((R.**) P.sigma (R.i64 2))                  -- sigma^2 * exp(1/2 * x * -1)
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
