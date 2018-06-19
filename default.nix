{ stdenv, fetchgit, gtk2, haskell, haskellPackages }:

let
  inherit (haskell) callHackage callCabal2nix;
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {
      xmonad = self.callCabal2nix "xmonad" (fetchgit { url = "git://github.com/xmonad/xmonad"; rev = "ecf1a0ca0d094c76a18c2c1b77ac7c1dcac10f5e"; sha256 = null; }) {};
      xmonad-contrib = self.callCabal2nix "xmonad-contrib" ./xmonad-contrib {};
      taffybar-ben = import ./taffybar-ben {};
      xmonad-ben = self.callCabal2nix "xmonad-ben" ./xmonad-ben {};
    };
  };

in
  haskellPkgs.xmonad-ben
