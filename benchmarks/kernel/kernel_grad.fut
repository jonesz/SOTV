import "../../lib/github.com/athas/vector/vector"
import "../../lib/github.com/athas/vector/vspace"
import "../../lib/github.com/jonesz/SOTV/kernel"

module vector_100 = any_vector {def length = 100i64}
module vspace_100_f32 = mk_vspace vector_100 f32
module P = { def l = 3.5f32
			 def sigma = 1.32f32 }
module K = mk_kernel_se f32 vector_100 vspace_100_f32 P

-- ==
-- entry: bench_kernel_grad
-- random input { [1000][100]f32 [1000][100]f32 }
-- random input { [10000][100]f32 [10000][100]f32 }
entry bench_kernel_grad [n] x_0 x_1 : [n][100]f32 =
	map2 (K.grad) x_0 x_1

-- ==
-- entry: bench_kernel_vjp
-- random input { [1000][100]f32 [1000][100]f32 }
-- random input { [10000][100]f32 [10000][100]f32 }
entry bench_kernel_vjp x_0 x_1 =
	let f x = let (a, b) = x in K.kernel a b
	in map (\c -> vjp f c 1) (zip x_0 x_1)
