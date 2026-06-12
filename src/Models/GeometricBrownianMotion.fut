-- | Geometric Brownian Motion: risk-neutral path simulation and
-- Monte Carlo valuation (Miletus' MonteCarloModel)

import "lib/github.com/diku-dk/cpprandom/random"
import "Payoff"

module norm_dist = normal_distribution f64 minstd_rand

type rng = minstd_rand.rng

def rng_from_seed (seed: i32): rng = minstd_rand.rng_from_seed [seed]

-- | n sample paths with m steps each, sampled at times (j+1)*t/m.
-- Log-Euler (exact) scheme: S_{j+1} = S_j * exp((kappa - sigma^2/2) dt + sigma sqrt(dt) Z)
-- where kappa = r - b is the risk-neutral drift.
def gbmPaths (n: i64) (m: i64) (seed: i32)
             (kappa: f64) (sigma: f64) (s_0: f64) (t: f64): [n][m]f64 =
  let dt = t / f64.i64 m
  let drift = (kappa - 0.5*sigma*sigma) * dt
  let vol = sigma * f64.sqrt dt
  let rngs = minstd_rand.split_rng (n*m) (rng_from_seed seed)
  let (_, zs) = unzip (map (norm_dist.rand {mean=0, stddev=1}) rngs)
  let logincs = map (\z -> drift + vol*z) zs
  in unflatten logincs |> map (scan (+) 0) |> map (map (\w -> s_0 * f64.exp w))

-- | European option value as the discounted average payoff over terminal values
def mcEuropean (kind: i8)
               (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
               (npaths: i64) (nsteps: i64) (seed: i32): f64 =
  let pth = gbmPaths npaths nsteps seed (r - b) sigma s0 t
  let terminal = map (\row -> row[nsteps-1]) pth
  let mean = reduce (+) 0 (map (payoff kind k) terminal) / f64.i64 npaths
  in f64.exp (-r*t) * mean
