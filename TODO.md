# Tickets 

* Compare results with geombm.jl (geombmPaths entry exists for this)
* yearfraction function, i.e. fix Time
* Volatility fitting
* SingleStock is sufficient, rather than X,Y,Z...?
* greeksWith now reads the option (kind/strike/expiry/underlying) from the
  contract + market, but only for vanilla europeans (AD runs through the scalar
  pricers). Greeks of the value process itself (AD through the lattice)?
✓ Functor, Applicative instances (Models.hs: P as a lattice value process)
✓ Value process threaded through Futhark: the Lattice engine reads strike,
  expiry and exercise dates from the contract; the elementwise combinators run
  in Haskell (Massiv) and the model operators (exch/disc/snell/absorb) in
  Futhark.
✓ Per-asset market data: the value process is priced against a Market
  environment (per-asset Quote of spot/vol/carry, plus rate and horizon); see
  Models.defaultMarket. (Horizon is still environment-wide, not from the dates.)
✓ Generic value-process models, one instance per representation: the Lattice
  instance is parameterized by binomial variant (CRR/JR/JRrn/Tian via Market),
  so each values arbitrary contracts and agrees with the scalar trees on
  vanillas. priceWith routes the binomial family through it.
✓ Paths instance (Monte Carlo / LSMC): the path-based value process in
  src/Models/Paths.fut — exch = GBM paths, disc/absorb = plain MC, snell =
  Longstaff-Schwartz over the value process. priceWith routes MC/LSMC there.
* Black-Scholes has no value process (closed-form); stays the vanilla fast path.
* The path snell uses the value process itself as the LSMC regressor (rather
  than a separate state variable); fine for vanillas, revisit for exotics.
✓ Greeks for european calls and puts
✓ Greeks autodiff (forward-mode jvp in Futhark, see src/Models/Greeks.fut)

# Models

* Heston Model?
✓ lattice : Cox Ross Rubinstein Model (CRRModel), plus JR, JRrn, Tian variants
✓ geombm in Futhark
✓ monte-carlo in Futhark
✓ Monte Carlo based American Option Pricing (Longstaff-Schwartz, src/Models/LSMC.fut)
✓ "Composing contracts" value-process lattice (paper §8, src/Models/Lattice.fut)

# General

✓ Test cases (cabal test: Miletus reference data + greeks vs closed form)
✓ Nix setup (flake.nix dev shell; Setup.hs runs futhark/futhask codegen)

# Ideas

✕ Implementation in Agda? See Valuation in paper (similar to contracts in Coq?)
✓ Futhark backend? Bind model to futhark? Reuse finpar?
