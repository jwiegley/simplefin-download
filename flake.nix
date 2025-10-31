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
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.simplefin-download.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          simplefin-download =
            final.haskell-nix.project' {
              src = ./.;
              supportHpack = true;
              compiler-nix-name = "ghc910";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
              };
              shell.buildInputs = with pkgs; [
                pkg-config
              ];
              modules = [{
                enableLibraryProfiling = true;
                enableProfiling = true;
              }];
            };
        })
      ];
    in flake // {
      packages.default = flake.packages."simplefin-download:exe:simplefin-download";

      devShell = pkgs.haskellPackages.shellFor {
        packages = p: [
        ];

        buildInputs = with pkgs.haskellPackages; [
          cabal-install
        ];

        withHoogle = true;
      };
    });
}
