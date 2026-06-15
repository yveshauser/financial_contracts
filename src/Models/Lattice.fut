-- | The lattice valuation model of "Composing contracts"
--
-- Futhark port of the value-process model from Peyton Jones, Eber & Seward,
-- "Composing contracts: an adventure in financial engineering" (§8), the same
-- model implemented on the GPU by Ahnfelt-Rønne & Werk.
--
-- A *value process* is a binomial lattice: one *slice* (random variable) per
-- time step, slice i holding the values across the states reachable at step i.
-- The lattice has `n` slices (n = steps + 1); node (i,j) is the state after j
-- up and i-j down moves. We store the (lower-triangular) lattice as an n*n
-- array; the upper triangle j>i is unused.
--
-- The real value processes are threaded through here: the Haskell side builds
-- the boolean and value processes with the elementwise combinators and passes
-- them to the model operators below, which run the backward induction over the
-- *actual* processes (no placeholder option). The tree is a binomial geometric
-- random walk parameterised by the `variant` of Binomial.updp
-- (0 = CRR, 1 = JR, 2 = JRrn, 3 = Tian): `priceLattice` (exch) uses its up/down
-- factors and the operators use its risk-neutral up-probability p.

import "Binomial"  -- updp: per-variant up/down factors and up-probability

-- | `exch` ("one k"): the price process of one unit of the underlying, in the
-- given binomial `variant`. Node (i,j) = s0 * u^j * d^(i-j).
def priceLattice (variant: i8) (s0: f64) (r: f64) (b: f64) (sigma: f64) (t: f64)
                 (n: i64): [n][n]f64 =
  let dt = t / f64.i64 (n - 1)
  let (u, d, _) = updp variant (r - b) sigma dt
  in tabulate_2d n n (\i j -> s0 * u**(f64.i64 j) * d**(f64.i64 (i - j)))

-- | Per-node combination of the boolean process `bb` (1/0), the value process
-- `vv` and the discounted continuation `c`, by model operator:
--   0 = disc (when):    take the value where the boolean holds, else discount.
--   1 = snell (anytime): the Snell envelope, max of value and continuation
--                        where the boolean holds (early exercise), else hold.
--   2 = absorb (until):  knock out (worth 0) where the boolean holds, else hold.
def combine (mode: i8) (bb: f64) (vv: f64) (c: f64): f64 =
  if mode == 2 then (if bb != 0 then 0 else c)
  else if mode == 1 then (if bb != 0 then f64.max vv c else c)
  else (if bb != 0 then vv else c)

-- | Backward induction over the supplied boolean process `bs` and value
-- process `vs`, using the variant's risk-neutral up-probability p (the up move
-- raises j by one). At the terminal slice there is no continuation (c = 0),
-- except that `absorb` keeps the value on surviving paths.
def induct [n] (mode: i8) (variant: i8) (kappa: f64) (sigma: f64) (r: f64) (dt: f64)
           (bs: [n][n]f64) (vs: [n][n]f64): [n][n]f64 =
  let (_, _, p) = updp variant kappa sigma dt
  let dsc = f64.exp (-r * dt)
  let last = n - 1
  let term = map2 (\bb vv ->
                     if mode == 2 then (if bb != 0 then 0f64 else vv)
                     else combine mode bb vv 0f64)
                  bs[last] vs[last]
  let w = (tabulate_2d n n (\_ _ -> 0f64)) with [last] = term
  in loop w = w for ii in reverse (iota last) do
       let nxt = w[ii+1]
       let cont = tabulate n (\j -> if j < last then dsc*(p*nxt[j+1] + (1-p)*nxt[j]) else 0f64)
       let row = map3 (combine mode) bs[ii] vs[ii] cont
       in w with [ii] = row

-- | `disc` ("when o c"): discount the value process back through the lattice.
def disc [n] (variant: i8) (kappa: f64) (sigma: f64) (r: f64) (dt: f64)
         (bs: [n][n]f64) (vs: [n][n]f64): [n][n]f64 =
  induct 0 variant kappa sigma r dt bs vs

-- | `snell` ("anytime o c"): the Snell envelope, allowing early exercise.
def snell [n] (variant: i8) (kappa: f64) (sigma: f64) (r: f64) (dt: f64)
          (bs: [n][n]f64) (vs: [n][n]f64): [n][n]f64 =
  induct 1 variant kappa sigma r dt bs vs

-- | `absorb` ("until o c"): a knock-out contract, worthless once the boolean
-- process becomes true; surviving paths are still discounted.
def absorb [n] (variant: i8) (kappa: f64) (sigma: f64) (r: f64) (dt: f64)
           (bs: [n][n]f64) (vs: [n][n]f64): [n][n]f64 =
  induct 2 variant kappa sigma r dt bs vs
