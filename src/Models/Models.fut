-- | Entry points for the pricing models (the model set of Miletus.jl)
--
-- Common flags:
--   kind:     1 = call, 0 = put
--   variant:  0 = CRR, 1 = JR, 2 = JRrn, 3 = Tian
--   exercise: 0 = european, 1 = american
-- b is the carry rate; the risk-neutral drift is kappa = r - b.

import "BlackScholes"
import "Binomial"
import "GeometricBrownianMotion"
import "LSMC"
import "Greeks"

-- | Analytic Black-Scholes (Miletus: GeomBMModel)
entry blackScholes (kind: i8)
                   (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64): f64 =
  bs kind s0 k r b sigma t

-- | Binomial trees (Miletus: BinomialGeomRWModel / CRRModel, JRModel, JRrnModel, TianModel)
entry binomial (variant: i8) (exercise: i8) (kind: i8)
               (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
               (steps: i64): f64 =
  binomial variant exercise kind s0 k r b sigma t steps

-- | Monte Carlo over GBM paths (Miletus: MonteCarloModel)
entry mcEuropean (kind: i8)
                 (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
                 (npaths: i64) (nsteps: i64) (seed: i32): f64 =
  mcEuropean kind s0 k r b sigma t npaths nsteps seed

-- | Longstaff-Schwartz least-squares Monte Carlo (Miletus: LeastSquaresMonteCarlo)
entry lsmcAmerican (kind: i8)
                   (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
                   (npaths: i64) (nsteps: i64) (degree: i64) (seed: i32): f64 =
  lsmcAmerican kind s0 k r b sigma t npaths nsteps degree seed

-- | Raw GBM sample paths, for inspection and comparison with geombm.jl
entry geombmPaths (n: i64) (m: i64) (seed: i32)
                  (kappa: f64) (sigma: f64) (s_0: f64) (t: f64): [n][m]f64 =
  gbmPaths n m seed kappa sigma s_0 t

-- | Greeks by automatic differentiation of the pricing functions.
-- blackScholesGreeks: [value, delta, gamma, vega, theta, rho]
-- the others:         [value, delta, vega, theta, rho]
-- (no AD gamma for tree/MC prices: they are piecewise linear in s0)
entry blackScholesGreeks (kind: i8)
                         (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64): [6]f64 =
  bsGreeks kind s0 k r b sigma t

entry binomialGreeks (variant: i8) (exercise: i8) (kind: i8)
                     (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
                     (steps: i64): [5]f64 =
  binomialGreeks variant exercise kind s0 k r b sigma t steps

entry mcGreeks (kind: i8)
               (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
               (npaths: i64) (nsteps: i64) (seed: i32): [5]f64 =
  mcGreeks kind s0 k r b sigma t npaths nsteps seed
