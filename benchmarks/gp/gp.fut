import "../../lib/github.com/jonesz/SOTV/gp"
import "../../lib/github.com/jonesz/SOTV/mean"
import "../../lib/github.com/jonesz/SOTV/kernel"

module U = mk_mean_zero f32
module A = mk_kernel_se f32 { def l = 2.0f32
							  def sigma = 1.0f32 }
module GP = mk_gp f32 U A

-- ==
-- entry: bench_gp_f32
-- random input { [10][10]f32    [10]f32   [100][10]f32 }
-- random input { [10][100]f32   [10]f32   [100][100]f32 }
-- random input { [1000][10]f32  [1000]f32 [1000][10]f32 }
-- random input { [1000][100]f32 [1000]f32 [1000][100]f32 }
entry bench_gp_f32 X Y x =
	map (GP.predictive_dist X Y) x
