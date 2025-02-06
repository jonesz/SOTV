import "kernel"
import "mean"
import "gp"
import "../../athas/vector/vector"
import "../../athas/vector/vspace"

module K = mk_kernel_se f32 { def l = 2f32
							 def sigma = 2f32 } 
											
module U = mk_mean_zero f32
module GP = mk_gp f32 U K
