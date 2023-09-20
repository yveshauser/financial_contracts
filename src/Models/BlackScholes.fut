-- | Black-Scholes solution for European Call Option
def call (s: f64) (t: f64) (k: f64) (r: f64) (sigma: f64) (m: f64) =
  let d1 = (f64.log (s/k) + (r + 0.5*sigma**2)*(m-t))/(sigma*f64.sqrt(m-t))
  let d2 = d1 - sigma*f64.sqrt(m-t)
  let n1 = 0.5*(1+ f64.erf(d1/f64.sqrt 2))
  let n2 = 0.5*(1+ f64.erf(d2/f64.sqrt 2))
  in s*n1 - k*f64.exp(-r*(m-t))*n2

-- | Black-Scholes solution for European Put Option
def put (s: f64) (t: f64) (k: f64) (r: f64) (sigma: f64) (m: f64) =
  let d1 = (f64.log (s/k) + (r + 0.5*sigma**2)*(m-t))/(sigma*f64.sqrt(m-t))
  let d2 = d1 - sigma*f64.sqrt(m-t)
  let n1 = 0.5*(1+ f64.erf(-d1/f64.sqrt 2))
  let n2 = 0.5*(1+ f64.erf(-d2/f64.sqrt 2))
  in k*f64.exp(-r*(m-t))*n2 - s*n1
