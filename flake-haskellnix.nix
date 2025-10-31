{
  description = "SimpleFin API client";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          simplefin-download =
            final.haskell-nix.project' {
              # Use the current directory which contains cabal.project
              # that references both . and ../trade-journal
              src = ./.;

              # Support hpack for package.yaml
              supportHpack = true;

              # GHC version
              compiler-nix-name = "ghc910";

              # Development tools
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                hlint = {};
              };

              # System dependencies including C libraries for simple-amount
              shell.buildInputs = with pkgs; [
                pkg-config
                gmp          # GNU Multiple Precision Arithmetic Library
                mpfr         # GNU MPFR Library for multiple-precision floating-point
                zlib         # Compression library (often needed)
              ];

              # Additional shell configuration
              shell.withHoogle = true;

              # Profiling support
              modules = [{
                enableLibraryProfiling = true;
                enableProfiling = true;

                # Ensure packages can find C libraries
                packages = {
                  simple-amount = {
                    # These will be passed to the package's configure script
                    configureFlags = [
                      "--extra-lib-dirs=${pkgs.gmp}/lib"
                      "--extra-include-dirs=${pkgs.gmp.dev}/include"
                      "--extra-lib-dirs=${pkgs.mpfr}/lib"
                      "--extra-include-dirs=${pkgs.mpfr.dev}/include"
                    ];
                  };
                };
              }];
            };
        })
      ];

      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };

      # Get the flake from the project
      flake = pkgs.simplefin-download.flake {};

    in flake // {
      # Default package is the main executable
      packages.default = flake.packages."simplefin-download:exe:simplefin-download";

      # Also expose the library and test suite if needed
      packages.lib = flake.packages."simplefin-download:lib:simplefin-download";
      packages.tests = flake.packages."simplefin-download:test:simplefin-download-test";

      # Default dev shell from haskell.nix flake
      # This properly includes all dependencies, tools, and C libraries
      devShells.default = flake.devShells.default;
    });
}
