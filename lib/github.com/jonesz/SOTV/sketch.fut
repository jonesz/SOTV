def sketch_fn 'a
              (mul: a -> a -> a)
              (add: a -> a -> a)
              (zero: a)
              (A_fn: i64 -> i64 -> a)
              (rand: i64 -> a)
              (m: i64)
              (n: i64)
              (r: i64) : [m][r]a =
  map (\m_idx ->                               -- (iota m), A's rows.
         map (\r_idx ->                        -- (iota r), each of S's cols.
                map (\t ->                     -- (iota t), A's cols/S's rows.
                       mul (A_fn m_idx t) (r_idx * n + t |> rand))
                    (iota n)
                |> reduce_comm add zero)
             (iota r))
      (iota m)

def sketch 'a [m] [n]
           (mul: a -> a -> a)
           (add: a -> a -> a)
           (zero: a)
           (A: [m][n]a)
           (rng: i64 -> a)
           (r: i64) : [m][r]a =
  let A_fn j k = A[j][k]
  in sketch_fn mul add zero A_fn rng m n r
