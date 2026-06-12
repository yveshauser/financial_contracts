-- | Binomial geometric random walk models
--
-- Futhark port of Miletus' BinomialGeomRWModel with its four constructors.
-- variant: 0 = CRR (Cox-Ross-Rubinstein), 1 = JR (Jarrow-Rudd),
--          2 = JRrn (Jarrow-Rudd risk-neutral), 3 = Tian
--
-- Reference: Finanzderivate mit Matlab, Michael Günther, Ansgar Jüngel

import "Payoff"

-- | Up/down factors and up-probability for one time step.
-- kappa = r - b is the risk-neutral drift.
def updp (variant: i8) (kappa: f64) (sigma: f64) (dt: f64): (f64, f64, f64) =
  let growth = f64.exp (kappa * dt)
  in if variant == 0 then
       let u = f64.exp (sigma * f64.sqrt dt)
       let d = 1/u
       in (u, d, (growth - d)/(u - d))
     else if variant == 1 then
       let u = f64.exp ((kappa - 0.5*sigma*sigma)*dt + sigma*f64.sqrt dt)
       let d = f64.exp ((kappa - 0.5*sigma*sigma)*dt - sigma*f64.sqrt dt)
       in (u, d, 0.5)
     else if variant == 2 then
       let u = f64.exp ((kappa - 0.5*sigma*sigma)*dt + sigma*f64.sqrt dt)
       let d = f64.exp ((kappa - 0.5*sigma*sigma)*dt - sigma*f64.sqrt dt)
       in (u, d, (growth - d)/(u - d))
     else
       let v = f64.exp (sigma*sigma*dt)
       let w = f64.sqrt (v*v + 2*v - 3)
       let u = 0.5 * growth * v * (v + 1 + w)
       let d = 0.5 * growth * v * (v + 1 - w)
       in (u, d, (growth - d)/(u - d))

-- | Backward induction on the tree.
-- exercise: 1 = american (early exercise at every node), 0 = european
def binomial (variant: i8) (exercise: i8) (kind: i8)
             (s0: f64) (k: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
             (n: i64): f64 =
  let dt = t / f64.i64 n
  let (u, d, p) = updp variant (r - b) sigma dt
  let dsc = f64.exp (-r * dt)
  let price (lvl: i64) (j: i64): f64 = s0 * u**(f64.i64 j) * d**(f64.i64 (lvl - j))
  let v0 = tabulate (n+1) (\j -> payoff kind k (price n j))
  let v = loop v = v0 for ii in reverse (iota n) do
    -- after this iteration v[0:ii+1] holds the values at tree level ii
    let m = ii + 1
    let cont = tabulate m (\j -> dsc * (p * v[j+1] + (1-p) * v[j]))
    let new = if exercise == 1
              then map2 f64.max cont (tabulate m (\j -> payoff kind k (price ii j)))
              else cont
    in v with [0:m] = new
  in v[0]
