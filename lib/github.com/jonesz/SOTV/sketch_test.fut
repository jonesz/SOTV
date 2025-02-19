import "sketch"

-- ==
-- entry: sketch_row_vector
-- input { [[1, 2], [3, 4]] [[1], [2]] }
-- output { [[5], [11]] }
entry sketch_row_vector A B =
  let rand j k = B[j][k]
  in sketch (*) (+) 0 A rand 1

-- ==
-- entry: sketch_matrix
-- input { [[1, 2, 3], [3, 4, 5], [6, 7, 8]] [[3, 4], [4, 5], [6, 7]] }
-- output { [[29, 35], [55, 67], [94, 115]] }
entry sketch_matrix A B =
  let rand j k = B[j][k]
  in sketch (*) (+) 0 A rand 2
