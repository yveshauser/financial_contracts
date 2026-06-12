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
    in
    {
      devShells = forAllSystems (system:
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
        {
          default = pkgs.mkShell {
            packages = [
              ghc
              hsPkgs.cabal-install
              hsPkgs.haskell-language-server
              pkgs.futhark
              futhask
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
