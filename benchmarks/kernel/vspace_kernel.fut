import "../../lib/github.com/athas/vector/vector"
import "../../lib/github.com/athas/vector/vspace"
import "../../lib/github.com/diku-dk/linalg/linalg"
import "../../lib/github.com/jonesz/SOTV/kernel"

module vector_2 = cat_vector vector_1 vector_1
module vector_3 = cat_vector vector_2 vector_1
module vector_5 = cat_vector vector_3 vector_2
module vector_10  = any_vector { def length = 10i64 }
module vector_100 = any_vector { def length = 100i64 }

module vspace_2_f32   = mk_vspace vector_2 f32
module vspace_3_f32   = mk_vspace vector_3 f32
module vspace_5_f32   = mk_vspace vector_5 f32
module vspace_10_f32  = mk_vspace vector_10 f32
module vspace_100_f32 = mk_vspace vector_100 f32

module K_vspace_2   = mk_kernel_se f32 vspace_2_f32 { def l = 2f32
										   			  def sigma = 2f32 }
module K_vspace_3   = mk_kernel_se f32 vspace_3_f32 { def l = 2f32
										   			  def sigma = 2f32 }
module K_vspace_5   = mk_kernel_se f32 vspace_5_f32 { def l = 2f32
										   			  def sigma = 2f32 }
module K_vspace_10  = mk_kernel_se f32 vspace_10_f32 { def l = 2f32
										   	           def sigma = 2f32 }
module K_vspace_100 = mk_kernel_se f32 vspace_100_f32 { def l = 2f32
										   	            def sigma = 2f32 }

module K_arr_2_f32 = mk_kernel_se_arr f32 { def l = 2f32
										    def sigma = 2f32
											def dim = 2i64 }

module K_arr_3_f32 = mk_kernel_se_arr f32 { def l = 2f32
										    def sigma = 2f32
											def dim = 3i64 }

module K_arr_5_f32 = mk_kernel_se_arr f32 { def l = 2f32
										    def sigma = 2f32
											def dim = 5i64 }

module K_arr_10_f32 = mk_kernel_se_arr f32 { def l = 2f32
										   	def sigma = 2f32
											def dim = 10i64 }

module K_arr_100_f32 = mk_kernel_se_arr f32 { def l = 2f32
										   	def sigma = 2f32
											def dim = 100i64 }

-- ==
-- entry: bench_vspace_2_f32 
-- random input { [1000][2]f32 [1000][2]f32 }
-- random input { [10000][2]f32 [10000][2]f32 }
-- random input { [100000][2]f32 [100000][2]f32 }
entry bench_vspace_2_f32 x x' =
	let x  = map (vector_2.from_array) x 
	let x' = map (vector_2.from_array) x'
	in map2 (K_vspace_2.K) x x'

-- ==
-- entry: bench_arr_2_f32 
-- random input { [1000][2]f32 [1000][2]f32 }
-- random input { [10000][2]f32 [10000][2]f32 }
-- random input { [100000][2]f32 [100000][2]f32 }
entry bench_arr_2_f32 x x' =
	map2 (K_arr_2_f32.K) x x'

-- ==
-- entry: bench_vspace_3_f32 
-- random input { [1000][3]f32 [1000][3]f32 }
-- random input { [10000][3]f32 [10000][3]f32 }
-- random input { [100000][3]f32 [100000][3]f32 }
entry bench_vspace_3_f32 x x' =
	let x  = map (vector_3.from_array) x 
	let x' = map (vector_3.from_array) x'
	in map2 (K_vspace_3.K) x x'

-- ==
-- entry: bench_arr_3_f32 
-- random input { [1000][3]f32 [1000][3]f32 }
-- random input { [10000][3]f32 [10000][3]f32 }
-- random input { [100000][3]f32 [100000][3]f32 }
entry bench_arr_3_f32 x x' =
	map2 (K_arr_3_f32.K) x x'

-- ==
-- entry: bench_vspace_5_f32 
-- random input { [1000][5]f32 [1000][5]f32 }
-- random input { [10000][5]f32 [10000][5]f32 }
-- random input { [100000][5]f32 [100000][5]f32 }
entry bench_vspace_5_f32 x x' =
	let x  = map (vector_5.from_array) x 
	let x' = map (vector_5.from_array) x'
	in map2 (K_vspace_5.K) x x'

-- ==
-- entry: bench_arr_5_f32 
-- random input { [1000][5]f32 [1000][5]f32 }
-- random input { [10000][5]f32 [10000][5]f32 }
-- random input { [100000][5]f32 [100000][5]f32 }
entry bench_arr_5_f32 x x' =
	map2 (K_arr_5_f32.K) x x'

-- ==
-- entry: bench_vspace_10_f32 
-- random input { [1000][10]f32 [1000][10]f32 }
-- random input { [10000][10]f32 [10000][10]f32 }
-- random input { [100000][10]f32 [100000][10]f32 }
entry bench_vspace_10_f32 x x' =
	let x  = map (vector_10.from_array) x 
	let x' = map (vector_10.from_array) x'
	in map2 (K_vspace_10.K) x x'

-- ==
-- entry: bench_arr_10_f32 
-- random input { [1000][10]f32 [1000][10]f32 }
-- random input { [10000][10]f32 [10000][10]f32 }
-- random input { [100000][10]f32 [100000][10]f32 }
entry bench_arr_10_f32 x x' =
	map2 (K_arr_10_f32.K) x x'

-- ==
-- entry: bench_vspace_100_f32 
-- random input { [1000][100]f32 [1000][100]f32 }
-- random input { [10000][100]f32 [10000][100]f32 }
-- random input { [100000][100]f32 [100000][100]f32 }
entry bench_vspace_100_f32 x x' =
	let x  = map (vector_100.from_array) x 
	let x' = map (vector_100.from_array) x'
	in map2 (K_vspace_100.K) x x'

-- ==
-- entry: bench_arr_100_f32 
-- random input { [1000][100]f32 [1000][100]f32 }
-- random input { [10000][100]f32 [10000][100]f32 }
-- random input { [100000][100]f32 [100000][100]f32 }
entry bench_arr_100_f32 x x' =
	map2 (K_arr_100_f32.K) x x'
