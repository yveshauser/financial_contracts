-- | Entry points for the pricing models (the model set of Miletus.jl)
--
-- Common flags:
--   kind:     1 = call, 0 = put
--   variant:  0 = CRR, 1 = JR, 2 = JRrn, 3 = Tian
--   exercise: 0 = european, 1 = american
-- b is the carry rate; the risk-neutral drift is kappa = r - b.

import "BlackScholes"
import "Binomial"
import "Lattice"
import "Paths"
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

-- | "Composing contracts" value-process lattice (Peyton Jones et al. §8): the
-- model operators driving `one` (exch -> priceLattice), `when` (disc),
-- `anytime` (snell) and `until` (absorb). The Haskell side builds the boolean
-- and value processes and threads them through here as n*n lattices.
entry priceLattice (variant: i8) (s0: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
                   (n: i64): [n][n]f64 =
  priceLattice variant s0 r b sigma t n

entry disc [n] (variant: i8) (kappa: f64) (sigma: f64) (r: f64) (dt: f64)
           (bs: [n][n]f64) (vs: [n][n]f64): [n][n]f64 =
  disc variant kappa sigma r dt bs vs

entry snell [n] (variant: i8) (kappa: f64) (sigma: f64) (r: f64) (dt: f64)
            (bs: [n][n]f64) (vs: [n][n]f64): [n][n]f64 =
  snell variant kappa sigma r dt bs vs

entry absorb [n] (variant: i8) (kappa: f64) (sigma: f64) (r: f64) (dt: f64)
             (bs: [n][n]f64) (vs: [n][n]f64): [n][n]f64 =
  absorb variant kappa sigma r dt bs vs

-- | Path-based value process (Monte Carlo / Longstaff-Schwartz): the operators
-- driving `one` (pricePaths), `when` (discPaths), `anytime` (snellPaths) and
-- `until` (absorbPaths) over an [npaths][n] value process.
entry pricePaths (s0: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
                 (npaths: i64) (n: i64) (seed: i32): [npaths][n]f64 =
  pricePaths npaths n seed (r - b) sigma s0 t

entry discPaths [np][m] (r: f64) (dt: f64) (bs: [np][m]f64) (vs: [np][m]f64): [np][m]f64 =
  discPaths r dt bs vs

entry snellPaths [np][m] (degree: i64) (r: f64) (dt: f64)
                 (bs: [np][m]f64) (vs: [np][m]f64): [np][m]f64 =
  snellPaths degree r dt bs vs

entry absorbPaths [np][m] (r: f64) (dt: f64) (bs: [np][m]f64) (vs: [np][m]f64): [np][m]f64 =
  absorbPaths r dt bs vs

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
