let
  nixpkgs = import ./nixpkgs.nix {};
  things = nixpkgs.callPackage (import ./default.nix) {
    inherit (nixpkgs) stdenv haskell;
    haskellPackages = nixpkgs.haskell.packages.ghc843;
  };
in things
