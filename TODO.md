# Tickets 

* Compare results with geombm.jl (geombmPaths entry exists for this)
* yearfraction function, i.e. fix Time
* Take option parameters from the contract (strike, expiry) instead of the
  placeholders in Models.hs (priceWith/greeksWith)
* Volatility fitting
* SingleStock is sufficient, rather than X,Y,Z...?
* Functor, Applicative instances?
✓ Greeks for european calls and puts
✓ Greeks autodiff (forward-mode jvp in Futhark, see src/Models/Greeks.fut)

# Models

* Heston Model?
✓ lattice : Cox Ross Rubinstein Model (CRRModel), plus JR, JRrn, Tian variants
✓ geombm in Futhark
✓ monte-carlo in Futhark
✓ Monte Carlo based American Option Pricing (Longstaff-Schwartz, src/Models/LSMC.fut)

# General

✓ Test cases (cabal test: Miletus reference data + greeks vs closed form)
✓ Nix setup (flake.nix dev shell; Setup.hs runs futhark/futhask codegen)

# Ideas

✕ Implementation in Agda? See Valuation in paper (similar to contracts in Coq?)
✓ Futhark backend? Bind model to futhark? Reuse finpar?
