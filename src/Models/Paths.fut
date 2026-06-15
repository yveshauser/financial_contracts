-- | The path-based value-process model (Monte Carlo / Longstaff-Schwartz)
--
-- A *value process* here is an [npaths][n] array: row = simulated path, column s
-- = time s*dt (n = nsteps + 1 time points; column 0 is today). The Haskell side
-- builds the boolean and value processes with the elementwise combinators and
-- threads them through the operators below.
--
--   exch   -> `pricePaths`: the simulated GBM price paths.
--   disc   -> `discPaths`:  on each path, the discounted value of receiving the
--             contract at the first time the boolean holds (plain Monte Carlo).
--   absorb -> `absorbPaths`: knock out (worth 0) once the boolean holds.
--   snell  -> `snellPaths`: Longstaff-Schwartz least-squares Monte Carlo, the
--             only operator that looks across paths (regression).

import "GeometricBrownianMotion"  -- gbmPaths
import "LSMC"                      -- fitPoly

-- | `exch` ("one k"): GBM price paths. Column 0 is the spot, column s the price
-- at time s*dt. kappa = r - b is the risk-neutral drift.
def pricePaths (npaths: i64) (n: i64) (seed: i32)
               (kappa: f64) (sigma: f64) (s0: f64) (t: f64): [npaths][n]f64 =
  let m = n - 1
  let inner = gbmPaths npaths m seed kappa sigma s0 t
  in map (\row -> tabulate n (\s -> if s == 0 then s0 else row[s-1])) inner

-- | `disc` ("when o c"): on each path, the discounted value of receiving the
-- value process at the first time the boolean process holds (else 0).
def discPaths [np][m] (r: f64) (dt: f64) (bs: [np][m]f64) (vs: [np][m]f64): [np][m]f64 =
  let dsc = f64.exp (-r * dt)
  in map2 (\b v ->
       let last = m - 1
       let w0 = (replicate m 0f64) with [last] = (if b[last] != 0 then v[last] else 0)
       in loop w = w0 for s in reverse (iota last) do
            w with [s] = if b[s] != 0 then v[s] else dsc * w[s+1])
     bs vs

-- | `absorb` ("until o c"): a knock-out contract, worth 0 once the boolean
-- holds; surviving paths are still discounted.
def absorbPaths [np][m] (r: f64) (dt: f64) (bs: [np][m]f64) (vs: [np][m]f64): [np][m]f64 =
  let dsc = f64.exp (-r * dt)
  in map2 (\b v ->
       let last = m - 1
       let w0 = (replicate m 0f64) with [last] = (if b[last] != 0 then 0 else v[last])
       in loop w = w0 for s in reverse (iota last) do
            w with [s] = if b[s] != 0 then 0 else dsc * w[s+1])
     bs vs

def poly [nb] (beta: [nb]f64) (x: f64): f64 =
  reduce (+) 0 (tabulate nb (\d -> beta[d] * x**(f64.i64 d)))

-- | `snell` ("anytime o c"): Longstaff-Schwartz. Working backwards in time, the
-- continuation is the discounted next-step value; at the nodes where exercise is
-- allowed and in the money, a least-squares regression of the realized
-- continuation on the value process decides whether to exercise (the value taken
-- is then the realized cashflow, never the regression estimate).
def snellPaths [np][m] (degree: i64) (r: f64) (dt: f64)
               (bs: [np][m]f64) (vs: [np][m]f64): [np][m]f64 =
  let dsc = f64.exp (-r * dt)
  let nb = degree + 1
  let bsT = transpose bs  -- time-major: [m][np]
  let vsT = transpose vs
  let last = m - 1
  let wlast = map2 (\b v -> if b != 0 then f64.max v 0 else 0) bsT[last] vsT[last]
  let wT = (replicate m (replicate np 0f64)) with [last] = wlast
  let wT = loop wT = wT for ss in reverse (iota last) do
             let realized = map (* dsc) wT[ss+1]
             let bb = bsT[ss]
             let vv = vsT[ss]
             let itm = map2 (\b v -> b != 0 && v > 0) bb vv
             let beta = fitPoly nb vv realized itm
             let row = map3 (\b v rc ->
                              if b != 0 && v > 0 && v > poly beta v then v else rc)
                            bb vv realized
             in wT with [ss] = row
  in transpose wT
