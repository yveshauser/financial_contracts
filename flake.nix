{
  description = "financial_contracts - composable financial contracts with Futhark-accelerated models";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hsPkgs = pkgs.haskellPackages;

          # The futhask release on Hackage (0.2.0) lacks the multicore backend
          # that src/Models/README.md relies on, so build the latest upstream
          # commit. doJailbreak lifts its stale base <4.15 bound.
          futhask = pkgs.haskell.lib.doJailbreak (hsPkgs.callCabal2nix "futhask"
            (pkgs.fetchFromGitLab {
              owner = "Gusten_Isfeldt";
              repo = "futhask";
              rev = "41967a7e4a08779c641d5b674d950b70919ab0f6";
              hash = "sha256-Kxj5MdhS6o2ObF16Fh1SKCI5Ly7prAYdfsifJP4UiII=";
            })
            { });

          # The single Futhark package dependency (src/Models/futhark.pkg).
          # Vendored so the custom Setup.hs skips `futhark pkg sync` (which would
          # need network, unavailable in the Nix build sandbox).
          cpprandom = pkgs.fetchFromGitHub {
            owner = "diku-dk";
            repo = "cpprandom";
            rev = "ec96adc06f7a91040b2c1f7f9fdc56b9b2e9f3a3";
            hash = "sha256-RSuqfOfTX2niXSVHc93OM9fyj6yTWxNKwEao0thvphY=";
          };

          # The library, built as a proper derivation: callCabal2nix + the
          # Futhark codegen tools, with cpprandom dropped into src/Models/lib so
          # Setup.hs regenerates the bindings offline.
          financial-contracts = pkgs.haskell.lib.addBuildTools
            (pkgs.haskell.lib.overrideCabal
              (hsPkgs.callCabal2nix "financial-contracts" self { })
              (old: {
                prePatch = (old.prePatch or "") + ''
                  rm -rf src/Models/lib
                  cp -r ${cpprandom}/lib src/Models/lib
                  chmod -R u+w src
                '';
                doCheck = false;
                doHaddock = false;
              }))
            [ pkgs.futhark futhask ];

          ghc = hsPkgs.ghcWithPackages (p: with p; [
            aeson
            bytestring
            deepseq
            erf
            logfloat
            massiv
            monad-control
            mtl
            mwc-random
            random
            text
            time
            transformers
            transformers-base
          ]);
        in
        { inherit pkgs hsPkgs futhask financial-contracts ghc; };
    in
    {
      # The library as a package, for downstream flakes to depend on directly.
      packages = forAllSystems (system:
        let s = perSystem system; in
        {
          default = s.financial-contracts;
          financial-contracts = s.financial-contracts;
        });

      devShells = forAllSystems (system:
        let s = perSystem system; in
        {
          default = s.pkgs.mkShell {
            packages = [
              s.ghc
              s.hsPkgs.cabal-install
              s.hsPkgs.haskell-language-server
              s.pkgs.futhark
              s.futhask
            ];

            shellHook = ''
              echo "financial_contracts dev shell"
              echo "  GHC:     $(ghc --numeric-version)"
              echo "  Futhark: $(futhark --version | head -n1)"
              echo
              echo "Build with: cabal build"
              echo "(Setup.hs regenerates the Futhark bindings when .fut files change)"
            '';
          };
        });
    };
}
