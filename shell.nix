let
  #nixpkgs = import ./nixpkgs.nix {};
  nixpkgs = import <nixpkgs> {};
  things = nixpkgs.callPackage (import ./default.nix) {
    inherit (nixpkgs) stdenv haskell;
    haskellPackages = nixpkgs.haskell.packages.ghc864;
  };
in things
