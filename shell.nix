{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (p: with p; [
      cabal-install
      hpack
      req
      aeson
      text
      bytestring
      base64-bytestring
      network-uri
      time
      mtl
      http-client
      http-types
      scientific
      transformers
    ]))
  ];

  shellHook = ''
    echo "SimpleFin Download Development Environment"
    echo "GHC version: $(ghc --version)"
    echo "Cabal version: $(cabal --version | head -1)"
    echo ""
    echo "To build: cabal build"
    echo "To run: cabal run simplefin-download"
  '';
}
