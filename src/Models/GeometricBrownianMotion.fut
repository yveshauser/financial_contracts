-- | Geometric Brownian Motion Model

import "lib/github.com/diku-dk/cpprandom/random"

module norm_dist = normal_distribution f64 minstd_rand

type rng = minstd_rand.rng

def time_steps (n: i64): [n]f64 = iota n |> map f64.i64
def cumsum [n] (x: [n]f64): [n]f64 = scan (+) 0.0 x

-- | Wiener process
def wiener [n] [m] (g: rng): [n][m]f64 =
  let rngs = minstd_rand.split_rng (n*m) g
  let (rngs, xs) = unzip (map (norm_dist.rand {mean=0, stddev=1}) rngs)
  let rng = minstd_rand.join_rng rngs
  in unflatten n m xs |> map cumsum

-- | Geometric Brownian Motion Model
def geombmModel [n] [m] (g: rng) (mu: f64) (sigma: f64) (s_0: f64): [n][m]f64 =
  let f (t, w) = s_0 * f64.exp ((((mu - sigma*sigma) / 2) * t) + (sigma * w))
  let indices = iota m |> map f64.i64 |> map (/100.0)
  in wiener g |> map (map f <-< zip indices)

entry main (n: i64) (m: i64) =
  let rng = minstd_rand.rng_from_seed [123]
  let o: [n][m]f64 = geombmModel rng 0 1 100
  in o
