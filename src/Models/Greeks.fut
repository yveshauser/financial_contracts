-- | Greeks via forward-mode automatic differentiation (jvp)
--
-- Each greek is the derivative of the model price with respect to one input,
-- computed by differentiating straight through the pricing code: the
-- analytic formula, the tree induction, or the Monte Carlo simulation
-- (pathwise derivative: the random numbers are held fixed).
--
-- Gamma is only provided for the analytic model: tree and Monte Carlo prices
-- are piecewise linear in s0 (through the payoff kink), so their AD second
-- derivative is 0 almost everywhere.

import "BlackScholes"
import "Binomial"
import "GeometricBrownianMotion"

def deriv (f: f64 -> f64) (x: f64): f64 = jvp f x 1
def deriv2 (f: f64 -> f64) (x: f64): f64 = jvp (\y -> jvp f y 1) x 1

-- | [value, delta, gamma, vega, theta, rho]
def bsGreeks (kind: i8) (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64): [6]f64 =
  let f s x r' t' = bs kind s k r' b x t'
  in [ f s0 sigma r t
     , deriv  (\x -> f x sigma r t) s0
     , deriv2 (\x -> f x sigma r t) s0
     , deriv  (\x -> f s0 x r t) sigma
     , -(deriv (\x -> f s0 sigma r x) t)
     , deriv  (\x -> f s0 sigma x t) r
     ]

-- | [value, delta, vega, theta, rho]
def binomialGreeks (variant: i8) (exercise: i8) (kind: i8)
                   (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
                   (steps: i64): [5]f64 =
  let f s x r' t' = binomial variant exercise kind s k r' b x t' steps
  in [ f s0 sigma r t
     , deriv (\x -> f x sigma r t) s0
     , deriv (\x -> f s0 x r t) sigma
     , -(deriv (\x -> f s0 sigma r x) t)
     , deriv (\x -> f s0 sigma x t) r
     ]

-- | [value, delta, vega, theta, rho]
def mcGreeks (kind: i8)
             (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
             (npaths: i64) (nsteps: i64) (seed: i32): [5]f64 =
  let f s x r' t' = mcEuropean kind s k r' b x t' npaths nsteps seed
  in [ f s0 sigma r t
     , deriv (\x -> f x sigma r t) s0
     , deriv (\x -> f s0 x r t) sigma
     , -(deriv (\x -> f s0 sigma r x) t)
     , deriv (\x -> f s0 sigma x t) r
     ]
