import "sketch"

-- ==
-- entry: sketch_row_vector
-- input { [[1, 2], [3, 4]] [1, 2] }
-- output { [[5], [11]] }
entry sketch_row_vector A b =
  let rand j = b[j]
  in sketch (*) (+) 0 A rand 2

-- ==
-- entry: sketch_matrix
-- input { [[1, 2, 3], [3, 4, 5], [6, 7, 8]] [[3, 2], [4, 5], [6, 7]] }
-- output { [[29, 33], [55, 61], [94, 103]] }
entry sketch_matrix A B =
  let B = flatten B
  let rand j = B[j]
  in sketch (*) (+) 0 A rand 2
