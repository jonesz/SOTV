import "../../lib/github.com/jonesz/SOTV/gp"
import "../../lib/github.com/jonesz/SOTV/mean"
import "../../lib/github.com/jonesz/SOTV/kernel"
import "../../lib/github.com/athas/vector/vector"
import "../../lib/github.com/athas/vector/vspace"

module vector_2 = cat_vector vector_1 vector_1
module vspace_2_f32 = mk_vspace vector_2 f32

module U = mk_mean_constant f32 vector_2 { def constant = 0f32 }
module A = mk_kernel_se f32 vector_2 vspace_2_f32 { def l = 2.0f32
							  						def sigma = 1.0f32 }
module GP = mk_direct_gp f32 vector_2 U A

-- ==
-- entry: bench_gp_f32
-- random input { [10][2]f32  [10]f32  [10][2]f32 }
-- random input { [100][2]f32 [100]f32 [100][2]f32 }
-- random input { [200][2]f32 [200]f32 [200][2]f32 }
entry bench_gp_f32 X Y x =
	let X = map (vector_2.from_array) X
	let x = map (vector_2.from_array) x
	let u = GP.compute_u_inv X Y
	in map (GP.predictive_dist u (zip X Y)) x
