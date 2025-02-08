import "../../athas/vector/vector"
import "../../athas/vector/vspace"

module type kernel = {
  --| A row vector.
  type v

  --| A scalar type.
  type s

  --| k(x_0, x_1).
  val kernel : v -> v -> s

  --| k'(x_0, x_1)
  val grad [n] : v -> v -> [n]s
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
    (S.-) x_0 x_1                                                  -- z := (x_0 - x_1)
    |> S.norm                                                      -- y := |z|^2
    |> (R.*) ((R.**) P.l (R.i64 2) |> (R.*) (R.i64 2) |> R.recip)  -- x := 1/2(l^2) * y
    |> R.neg                                                       -- -x
    |> R.exp                                                       -- exp(-x)
    |> (R.*) ((R.**) P.sigma (R.i64 2))                            -- sigma^2 * exp(-x)

  def grad [n] x_0 x_1 =
    -- In this implementation, `P.l` is constant across the matrix; this works
    -- better with the interface of `vspace` - we can utilize `scale`.
    -- let L = (replicate n P.l) |> map (flip (R.**) (R.i64 2)) |> map (R.recip) -- [n]t

    -- https://jungtaek.github.io/notes/derivatives_kernels.pdf
    let lhs =
      let z = (S.-) x_0 x_1                                    -- z := (x_0 - x_1)
      in S.scale ((R.**) P.l (R.i64 2) |> R.recip) z           -- y := (1/l^2) * z
        |> S.dot z                                             -- dot(y, z)
        |> (R.*) (R.recip (R.i64 2))                           -- (1/2) * dot(y, z)
        |> R.neg                                               -- -(1/2) * dot(y, z)
        |> R.exp                                               -- exp(-(1/2) * dot(y, z))
        |> (R.*) ((R.**) P.sigma (R.i64 2))                    -- s^2 * exp(-(1/2) * dot(y, z))

    let rhs =
      map (\k ->
        (V.get k x_1)                                          -- x_1[k]
        |> (R.-) (V.get k x_0)                                 -- x_0[k] - x_1[k]
        |> (R.**) (R.i64 2)                                    -- z := (x_0[k] - x_1[k])^2
        |> (R.*) (P.l |> flip (R.**) (R.i64 3) |> R.recip))    -- 1/l^3 * z
      (iota V.length)

    in map ((R.*) lhs) rhs :> [n]s
}

--| Rational Quadratic Kernel.
-- module mk_kernel_rq
--   (R: real)
--   (V: vector)
--   (S: vspace with vector = V.vector R.t with real = R.t)
--   (P: {
--     val l : R.t
--     val sigma : R.t
--     val a : R.t
--   })
--   : kernel with v = V.vector R.t with s = R.t = {
--   type v = V.vector R.t
--   type s = R.t
-- 
--   def kernel x x' =
--     -- (sigma^2)*(1+(||x-x'||^2)/(2al^2))^(-a)
--     (S.-) x x' |> S.norm |> flip (R./) ((R.**) P.l (R.i64 2) |> (R.*) P.a |> (R.*) (R.i64 2))
--     |> (R.+) (R.i64 1)
--     |> flip (R.**) (R.neg P.a)
--     |> (R.*) ((R.**) P.sigma (R.i64 2))
-- }
