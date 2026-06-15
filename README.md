# Financial Contracts DSL

Composable financial contracts in Haskell, priced by models written in
Futhark. Contracts are built from the combinators of Peyton Jones et al. [1]
and valued by a pricing model, in the spirit of [2] and [3].

## Build

~~~ bash
nix develop        # GHC, cabal, futhark, futhask
cabal build        # Setup.hs regenerates the Futhark bindings as needed
cabal test         # validates the models against Miletus' reference data
cabal run models -- bs   # or: crr | jr | jrrn | tian | mc | lsmc | lattice
~~~

## References

[1]: Peyton Jones, S., et al.: Composing contracts: An adventure in financial engineering, (functional pearl). In: Proceedings of the Fifth ACM SIGPLAN ICFP. ACM (2000)

[2]: Ahnfelt-Rønne, J., Flænø Werk, M: Pricing composable contracts on the GP-GPU, Department of Computer Science, University of Copenhagen, August 17, 2011

[3]: Miletus, https://github.com/JuliaComputing/Miletus.jl
