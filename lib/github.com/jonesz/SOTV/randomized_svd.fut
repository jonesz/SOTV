import "../../diku-dk/cpprandom/random"

module randomized_svd (R: real) (E: rng_engine) = {
  type t = R.t
  module dist = normal_distribution R E

  def sketch 'a [n] [k] rng f (x: [n]a) : (E.rng, [n][k]t) =
    -- TODO: If we utilized a CBRNG, we could compute the matrix of Gaussian noise on-demand;
    -- as a result, we wouldn't have to read/write to mem.
    let rngs = E.split_rng (n * k) rng
    let (rngs, omega) = map (dist.rand {mean = (R.f32 0), stddev = (R.f32 1.0)}) rngs |> unzip
    let omega: [n][k]t = unflatten omega
    let rng = E.join_rng rngs

    in ( rng
       , map (\x_i ->
                let x_row = map (\x_j -> f x_i x_j) x
                in map (\o_col -> map2 (R.*) x_row o_col |> reduce (R.+) (R.i64 0)) (transpose omega))
             x
       )
}
