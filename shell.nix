let
  nixpkgs = import <nixpkgs> {};
in
  nixpkgs.callPackage (import ./.) {
    inherit (nixpkgs) stdenv haskell;
    haskellPackages = nixpkgs.haskell.packages.ghc802;
  }
