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
module R = normal_distribution f32 minstd_rand

def knn k X Y x =
	let d a b = map2 (f32.-) a b |> map (flip (f32.**) 2f32) |> reduce (f32.+) 0f32 |> f32.sqrt
	in map (d x) X |> zip Y |> radix_sort_float_by_key (.1) f32.num_bits f32.get_bit
		|> take k |> map (.0) |> reduce (f32.+) 0f32 |> flip (f32./) (f32.i64 k)

def knn' k = knn (i64.f32 k[0])

entry example = 
	let X_len = 100000i64

	let rng = minstd_rand.rng_from_seed [123]
	let rngs = minstd_rand.split_rng X_len rng

	let E = map (R.rand {mean=0f32, stddev=3.0f32}) rngs |> map (.1)
	let X = map (\b -> [f32.i64 b]) (iota X_len)
	let Y = map2 (\a b -> (f32.i64 a) |> flip (f32./) 100f32 |> f32.sin |> (f32.+) b) (iota X_len) E

	let X_train = take (X_len / 2) X
	let Y_train = take (X_len / 2) Y
	let X_test  = reverse X |> take (X_len / 2)
	let Y_test  = reverse Y |> take (X_len / 2)

	let loss (k: [1]f32) =
		map (knn' k X_train Y_train) X_test |> map2 (f32.-) Y_test |> map (flip (f32.**) 2f32) |> reduce (f32.+) 0f32 |> f32.neg |> flip (f32./) (X_len / 2 |> f32.i64)

	let p = S.next loss 10i64 [3f32]
	in p.1
