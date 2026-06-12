-- | Least-squares Monte Carlo (Longstaff-Schwartz) for American options
--
-- Futhark port of Miletus' LeastSquaresMonteCarlo: backward induction over
-- GBM paths, estimating the continuation value at each step by polynomial
-- regression over the in-the-money paths.

import "Payoff"
import "GeometricBrownianMotion"

-- | Solve a (small, symmetric positive definite) linear system by Gaussian
-- elimination without pivoting. Returns zeros for singular pivots.
def solveLin [kk] (a0: [kk][kk]f64) (b0: [kk]f64): [kk]f64 =
  let (a, b) = loop (a, b) = (a0, b0) for i < kk do
    let fac = tabulate kk (\row -> if row > i && a[i,i] != 0 then a[row,i]/a[i,i] else 0)
    let a = tabulate_2d kk kk (\row col -> a[row,col] - fac[row]*a[i,col])
    let b = tabulate kk (\row -> b[row] - fac[row]*b[i])
    in (a, b)
  in loop x = replicate kk 0 for ii < kk do
       let i = kk - 1 - ii
       let s = reduce (+) 0 (tabulate kk (\j -> if j > i then a[i,j]*x[j] else 0))
       in x with [i] = if a[i,i] != 0 then (b[i] - s)/a[i,i] else 0

-- | Least-squares fit of a polynomial of degree nb-1 through the points
-- (xs, vs) selected by the mask, via the normal equations.
def fitPoly [np] (nb: i64) (xs: [np]f64) (vs: [np]f64) (mask: [np]bool): [nb]f64 =
  let m = tabulate_2d nb nb (\p q ->
            reduce (+) 0 (map2 (\x sel -> if sel then x**(f64.i64 (p+q)) else 0) xs mask))
  let rhs = tabulate nb (\p ->
            reduce (+) 0 (map3 (\x v sel -> if sel then v * x**(f64.i64 p) else 0) xs vs mask))
  in solveLin m rhs

def lsmcAmerican (kind: i8)
                 (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
                 (npaths: i64) (nsteps: i64) (degree: i64) (seed: i32): f64 =
  let pth = gbmPaths npaths nsteps seed (r - b) sigma s0 t
  let dsc = f64.exp (-r * (t / f64.i64 nsteps))
  let nb = degree + 1
  let v0 = map (\row -> payoff kind k row[nsteps-1]) pth
  let v = loop v = v0 for jj < nsteps - 1 do
    let j = nsteps - 2 - jj
    let v = map (* dsc) v
    let xs = pth[:, j]
    let es = map (payoff kind k) xs
    let itm = map (> 0) es
    in if !(or itm) then v
       else
         let beta = fitPoly nb xs v itm
         let cont = map (\x -> reduce (+) 0 (tabulate nb (\d -> beta[d] * x**(f64.i64 d)))) xs
         in map4 (\e c sel vi -> if sel && e > c then e else vi) es cont itm v
  in f64.max (payoff kind k s0)
             (dsc * reduce (+) 0 v / f64.i64 npaths)
