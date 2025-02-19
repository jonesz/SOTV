-- | Sketch functions. These are structured so that the matrix to be
-- approximated A, never has to be stored within mem, and the values of the
-- sketch matrix S are computed during the matmul.

def sketch_fn 'a
              (mul: a -> a -> a)
              (add: a -> a -> a)
              (zero: a)
              (A_fn: i64 -> i64 -> a)
              (rand: i64 -> i64 -> a)
              (m: i64)
              (n: i64)
              (r: i64) : [m][r]a =
  map (\m_idx ->                               -- (iota m), A's rows.
         map (\r_idx ->                        -- (iota r), each of S's cols.
                map (\n_idx ->                 -- (iota n), A's cols/S's rows.
                       mul (A_fn m_idx n_idx) (rand n_idx r_idx))
                    (iota n)
                |> reduce_comm add zero)
             (iota r))
      (iota m)

def sketch 'a [m] [n]
           (mul: a -> a -> a)
           (add: a -> a -> a)
           (zero: a)
           (A: [m][n]a)
           (rand: i64 -> i64 -> a)
           (r: i64) : [m][r]a =
  let A_fn j k = A[j][k]
  in sketch_fn mul add zero A_fn rand m n r
