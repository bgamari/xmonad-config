let
  #nixpkgs = import ./nixpkgs.nix {};
  #nixpkgs = import <nixpkgs> {};
  nixpkgs = import /opt/exp/nixpkgs {};
  things = nixpkgs.callPackage (import ./default.nix) {
    inherit (nixpkgs) stdenv haskell;
    haskellPackages = nixpkgs.haskell.packages.ghc865;
  };
in things
