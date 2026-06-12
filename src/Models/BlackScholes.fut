-- | Black-Scholes solutions for European options
--
-- `bs` corresponds to Miletus' GeomBMModel valuation: Black-Scholes on the
-- forward price, with a carry rate b (kappa = r - b is the risk-neutral
-- drift of the underlying).

def normcdf (x: f64): f64 = 0.5 * (1 + f64.erf (x / f64.sqrt 2))

-- | kind: 1 = call, 0 = put
def bs (kind: i8) (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64): f64 =
  let fwd = s0 * f64.exp ((r - b) * t)
  let sig = sigma * f64.sqrt t
  let d1 = (f64.log (fwd/k) + 0.5*sig*sig) / sig
  let d2 = d1 - sig
  in if kind == 1
     then f64.exp (-r*t) * (fwd * normcdf d1 - k * normcdf d2)
     else f64.exp (-r*t) * (k * normcdf (-d2) - fwd * normcdf (-d1))

-- | Black-Scholes solution for European Call Option (value at time t, expiry m)
def call (s: f64) (t: f64) (k: f64) (r: f64) (sigma: f64) (m: f64) =
  bs 1 s k r 0 sigma (m-t)

-- | Black-Scholes solution for European Put Option (value at time t, expiry m)
def put (s: f64) (t: f64) (k: f64) (r: f64) (sigma: f64) (m: f64) =
  bs 0 s k r 0 sigma (m-t)
