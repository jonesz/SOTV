import "kernel"
import "mean"
import "../../athas/vector/vector"
import "../../diku-dk/linalg/linalg"
import "../../diku-dk/linalg/lu"
import "../../jonesz/krylov-fut/cgm"
import "../../jonesz/sparse/symmetric"

module type gp = {
  --| A row vector.
  type v

  --| A scalar type.
  type s

  val solve_mu_inv [n] : [n]v -> [n]s -> [n]s
  -- TODO: autograd will not work with this...
  val solve_sigma_inv [n] : [n]v -> v -> [n]s

  --| Given a set of observations and a potential new point, return `(mu, stddev)`.
  val predictive_dist [n] : (mu_inv: [n]s) -> (sigma_inv: [n]s) -> [n]v -> v -> (s, s)
}

module mk_gp_cg_sym
  (R: real)
  (V: vector)
  (U: mean with v = V.vector R.t with s = R.t)
  (K: kernel with v = V.vector R.t with s = R.t)
  (S: symmetric_matrix with t = R.t)
  (C: cgm with t = R.t with m [n] = S.mat [n])
  : gp with v = V.vector R.t with s = R.t = {
  type v = V.vector R.t
  type s = R.t

  module L = mk_linalg R

  def GRAM [n] x : S.mat [n] =
    map (\k ->
           -- floor((sqrt(1 + 8k) -1) / 2)
           let i: i64 = (f32.*) 8f32 (f32.i64 k) |> (f32.+) 1f32 |> f32.sqrt |> flip (f32.-) 1f32 |> flip (f32./) 2f32 |> f32.floor |> i64.f32
           -- k - (i * (i + 1)) / 2
           let j = i + 1 |> (*) i |> flip (/) 2 |> (-) k
           in K.kernel x[i] x[j])
        (iota (n * (n + 1i64) / 2i64))
    |> S.symmetric

  def crosscov x x_next =
    map (K.kernel x_next) x

  def solve_mu_inv [n] x f =
    -- Ka = (Y - m(x))
    let b = map (U.u) x |> map2 (R.-) f
    let A = GRAM x
    in C.cgm A b (replicate n (R.i64 0)) (R.f32 0.0001f32) 1000i64

  def solve_sigma_inv [n] x x_next =
    -- Ka = k_star
    let b = crosscov x x_next
    let A = GRAM x
    in C.cgm A b (replicate n (R.i64 0)) (R.f32 0.0001f32) 1000i64

  def predictive_dist u sigma x x_next =
    let k_star = crosscov x x_next
    let u_t =
      L.dotprod k_star u |> (R.+) (U.u x_next)
    let stddev_t =
      L.dotprod k_star sigma |> (R.-) (K.kernel x_next x_next)
    in (u_t, stddev_t)
}
