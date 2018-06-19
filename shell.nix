let
  nixpkgs = import /home/ben/.nix-overlay/nixpkgs {};
in
  nixpkgs.callPackage (import ./default.nix) {
    inherit (nixpkgs) stdenv haskell;
    haskellPackages = nixpkgs.haskell.packages.ghc843;
  }
