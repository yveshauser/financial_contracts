-- Cox Ross Rubinstein Model
--
-- Reference: Finanzderivate mit Matlab, Michael Günther, Ansgar Jüngel
def crrModel (K:f64, S0:f64, r:f64, sigma:f64, T:f64, N:i64) =

  let dt:f64 = T/(f64.i64 N)
  let beta = 0.5*(f64.exp(-r*dt) + f64.exp((r+sigma*sigma)*dt))
  let u:f64 = beta + f64.sqrt(beta*beta-1.0)
  let d = 1/u
  let p = (f64.exp(r*dt)-d)/(u-d)
  let q = 1-p

  let S:[N]f64 =
    let us = tabulate N (\i -> u**(f64.i64 i))
    let ds = tabulate N (\i -> d**(f64.i64 i)) |> reverse
    in map2 (*) us ds |> map (*S0)

  let V:[N]f64 =
    let Ks = replicate N K
    in map2 (\k s -> f64.max (k-s) 0) Ks S

  in (loop V for i in (iota N |> reverse) do
    V with [0:i] =
      let ps = map (*p) V[1:i+1] :> [i]f64
      let qs = map (*q) V[0:i] :> [i]f64
      in map2 (+) ps qs) |> head |> (*f64.exp(-r*T))

-- Example from the refenced book, see Matlab-Programm 3.1 (p.38)
def main (n:i64) =
  let K:f64 = 6
  let S0:f64 = 5
  let r:f64 = 0.04
  let sigma:f64 = 0.3
  let T:f64 = 1
  in crrModel (K, S0, r, sigma, T, n)
