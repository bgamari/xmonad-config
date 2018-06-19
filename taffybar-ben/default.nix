{ nixpkgs ? (import ~/.nix-overlay/nixpkgs {}) }:

let
  inherit (nixpkgs.haskell.lib) enableDWARFDebugging;

  myHaskellPackages = nixpkgs.haskell.packages.ghc843.override {
    overrides = self: super: {
      taffybar = enableDWARFDebugging (import ../taffybar-new/shell.nix { inherit nixpkgs; compiler = "ghc843"; });
      taffybar-ben = enableDWARFDebugging (self.callCabal2nix "taffybar-ben" ./. {});
    };
  };
in myHaskellPackages.taffybar-ben
