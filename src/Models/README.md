# Models

Futhark implementations of the pricing models provided by
[Miletus.jl](https://github.com/JuliaComputing/Miletus.jl):

| File                          | Model                              | Miletus equivalent       |
|-------------------------------|------------------------------------|--------------------------|
| `BlackScholes.fut`            | analytic Black-Scholes (with carry)| `GeomBMModel`            |
| `Binomial.fut`                | binomial trees: CRR, JR, JRrn, Tian, European and American exercise | `BinomialGeomRWModel` (`CRRModel`, `JRModel`, `JRrnModel`, `TianModel`) |
| `GeometricBrownianMotion.fut` | GBM paths + Monte Carlo valuation  | `MonteCarloModel`        |
| `LSMC.fut`                    | Longstaff-Schwartz least-squares Monte Carlo for American options | `LeastSquaresMonteCarlo` |
| `Greeks.fut`                  | greeks by forward-mode AD (`jvp`) through the analytic, tree and MC pricers | `greeks.jl` (uses ForwardDiff) |
| `Payoff.fut`                  | vanilla call/put payoffs           |                          |
| `Models.fut`                  | entry points for the Haskell bindings |                       |

Not ported: Miletus' unexported experimental models (`geomou.jl`, `sabr.jl`,
`trinomial.jl`, `spline.jl`).

Sanity check (at-the-money European call, s0 = k = 100, r = 5%, sigma = 30%,
t = 1y): all models agree with the analytic value 14.2313; the American put
(same parameters) is 9.87 on the CRR tree and matches LSMC.

# build

`cabal build` generates everything automatically: Setup.hs reruns the steps
below whenever a `.fut` file is newer than the generated bindings.

Manual equivalent:

~~~ bash
futhark pkg sync   # fetch lib/ dependencies (first time only)
futhark multicore --library Models.fut
futhask multicore Models.h . Models.Futhark
mv Models.Futhark Futhark
mv Models.Futhark.hs Futhark.hs
~~~
