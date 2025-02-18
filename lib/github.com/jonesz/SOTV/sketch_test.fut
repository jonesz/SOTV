import "sketch"

-- entry: sketch_row_vector
-- input { [[1, 2], [3, 4]], [1, 2] }
-- output { [[5], [11]] }
def sketch_row_vector A b =
  let rand j = b[j]
  in sketch (*) (+) 0 A rand 2
