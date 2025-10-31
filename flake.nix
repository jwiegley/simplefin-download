{
  description = "SimpleFin API client with Ledger output";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # GHC with required packages
        haskellPackages = pkgs.haskell.packages.ghc910.override {
          overrides = self: super: {
            # Override simple-amount to include C library dependencies
            simple-amount = super.simple-amount.overrideAttrs (oldAttrs: {
              librarySystemDepends = (oldAttrs.librarySystemDepends or []) ++
                [ pkgs.gmp pkgs.mpfr ];
            });
          };
        };

        # Development environment packages
        devTools = with haskellPackages; [
          cabal-install
          haskell-language-server
          hlint
          hoogle
          ghcid
          ormolu
        ];

        # System dependencies
        systemDeps = with pkgs; [
          pkg-config
          gmp
          mpfr
          zlib
        ];

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [ haskellPackages.ghc ] ++ devTools ++ systemDeps;

          shellHook = ''
            echo "SimpleFin Download Development Environment"
            echo "=========================================="
            echo "GHC version: $(ghc --version)"
            echo "Cabal version: $(cabal --version | head -n1)"
            echo ""
            echo "Available commands:"
            echo "  cabal build       - Build the project"
            echo "  cabal test        - Run tests"
            echo "  cabal run         - Run simplefin-download"
            echo "  ghcid             - Auto-recompiling GHC daemon"
            echo "  haskell-language-server - LSP for IDE support"
            echo ""
            echo "System libraries available:"
            echo "  mpfr: ${pkgs.mpfr}/lib"
            echo "  gmp: ${pkgs.gmp}/lib"
            echo ""

            # Set up library paths for Cabal
            export LIBRARY_PATH="${pkgs.gmp}/lib:${pkgs.mpfr}/lib:$LIBRARY_PATH"
            export C_INCLUDE_PATH="${pkgs.gmp.dev}/include:${pkgs.mpfr.dev}/include:$C_INCLUDE_PATH"
            export PKG_CONFIG_PATH="${pkgs.gmp.dev}/lib/pkgconfig:${pkgs.mpfr.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
          '';
        };

        # Alternative minimal shell for quick work
        devShells.minimal = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            haskellPackages.cabal-install
            pkg-config
            gmp
            mpfr
          ];
        };
      });
}