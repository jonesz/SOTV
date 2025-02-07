import "../lib/github.com/jonesz/SOTV/sotv"
import "../lib/github.com/jonesz/SOTV/mean"
import "../lib/github.com/jonesz/SOTV/gp"
import "../lib/github.com/jonesz/SOTV/policy/ucb"
import "../lib/github.com/jonesz/SOTV/kernel"
import "../lib/github.com/jonesz/SOTV/policy/opt/gd"
import "../lib/github.com/diku-dk/cpprandom/random"
import "../lib/github.com/diku-dk/sorts/radix_sort"

module K = mk_kernel_se f32 { def l = 2f32
							  def sigma = 2f32 }
module U = mk_mean_zero f32 
module GP = mk_gp f32 U K 

module P = mk_ucb f32 GP { def kappa = 1f32 }
module GD = mk_gd f32

module S = mk_sotv f32 P GD

def knn k X Y x =
	let d a b = map2 (f32.-) a b |> map (flip (f32.**) 2f32) |> reduce (f32.+) 0f32 |> f32.sqrt
	in map (d x) X |> zip Y |> radix_sort_float_by_key (.1) f32.num_bits f32.get_bit
		|> take k |> map (.0) |> reduce (f32.+) 0f32 |> flip (f32./) (f32.i64 k)

def knn' k = knn (i64.f32 k)
