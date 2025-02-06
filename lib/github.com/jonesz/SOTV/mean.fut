import "../../athas/vector/vector"

module type mean = {
	--| A row vector.
	type v [n]
	--| A scalar type.
	type s

	val u [n] : v[n] -> s
}

-- module mk_mean_zero (R: real) (V: vector) : mean with v = V.vector R.t with s = R.t = {
-- 	type v = V.vector R.t
-- 	type s = R.t
-- 
-- 	def u _ = (R.i64 0) 
-- }

module mk_mean_zero (R: real) : mean with v[n] = [n]R.t with s = R.t = {
 	type v [n] = [n]R.t
 	type s = R.t
 
 	def u _ = (R.i64 0) 
 }
