{ stdenv, fetchgit, gtk2, haskell, haskellPackages }:

let
  inherit (haskell) callHackage callCabal2nix;
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {
      #cairo = self.callHackage "cairo" "0.13.4.1" {};
      #glib = self.callHackage "glib" "0.13.5.0" {};
      #gtk = self.callHackage "gtk" "0.14.7" {};
      #gtk3 = self.callHackage "gtk3" "0.14.8" {};
      #gtk2hs-buildtools = self.callHackage "gtk2hs-buildtools" "0.13.3.0" { Cabal = self.Cabal_1_24_2_0; };

      xmonad = self.callCabal2nix "xmonad" (fetchgit { url = "git://github.com/xmonad/xmonad"; rev = "ecf1a0ca0d094c76a18c2c1b77ac7c1dcac10f5e"; sha256 = null; }) {};
      xmonad-contrib = self.callCabal2nix "xmonad-contrib" ./xmonad-contrib {};
      #taffybar = self.callCabal2nix "taffybar" ./taffybar-new { gtk2 = gtk2; };
      xmonad-ben = self.callCabal2nix "xmonad-ben" ./xmonad-ben {};
    };
  };

in
  stdenv.mkDerivation {
    name = "xmonad-ben";
    buildInputs = with haskellPkgs; [ xmonad ];
    meta.license = stdenv.lib.licenses.bsd3;
    src = ./.;
    installPhase = ''
      mkdir -p $out/bin
    '';
  }
