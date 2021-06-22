{ stdenv, fetchurl, fetchgit, fetchFromGitHub,
  libdbusmenu-glib, libdbusmenu-gtk3, dbus, glib, gtk3, cairo, pango,
  gobject-introspection,
  haskell, haskellPackages, xorg
}:

let
  inherit (haskell) callCabal2nix;
  inherit (haskell.lib) enableDWARFDebugging enableExecutableProfiling
                        addPkgconfigDepend dontCheck doJailbreak addBuildDepends
                        unmarkBroken;

  libglib = glib;
  libgtk3 = gtk3;
  libcairo = cairo;

  all-cabal-hashes = 
    let rev = "38b1fe33b6f370c30a04ae7522f6a5c818472aff";
    in fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
      sha256 = "sha256:1vz8s1d7smz42x8sbzybaixmn7bnyv5y4qwk35m3mk9wbmxzq0yf";
    };

  haskellPkgs = haskellPackages.override {
    inherit all-cabal-hashes;

    overrides = self: super: 
      let
        inherit (self) callHackage;
      in rec {
      # Xmonad
      xmonad = self.callCabal2nix "xmonad" (fetchFromGitHub {
        name = "xmonad";
        owner = "xmonad";
        repo = "xmonad";
        rev = "bb13853929f8f6fc59b526bcc10631e1bac309ad";
        sha256 = "1f2w0vkv4i40sa52d0bxdhmn9zsikzymm91xwdi4m64nqwip1i97";
      }) {};

      gi-cairo-connector = callHackage "gi-cairo-connector" "0.1.0" {};
      gi-cairo-render = callHackage "gi-cairo-render" "0.1.0" { inherit cairo; };
      gi-gdkx11 = callHackage "gi-gdkx11" "3.0.10" { inherit gtk3; };
      gi-xlib = callHackage "gi-xlib" "2.0.9" { };
      gi-gtk-hs = callHackage "gi-gtk-hs" "0.3.9" { };
      gtk-sni-tray = unmarkBroken super.gtk-sni-tray;
      gi-dbusmenugtk3 = callHackage "gi-dbusmenugtk3" "0.4.9" { inherit gtk3; };
      gi-dbusmenu = callHackage "gi-dbusmenu" "0.4.8" {};
      gtk-strut   = unmarkBroken super.gtk-strut  ;

      xmonad-contrib = self.callCabal2nix "xmonad-contrib" ./xmonad-contrib {};
      xmonad-ben = self.callCabal2nix "xmonad-ben" ./xmonad-ben {};
      inherit (import ./taffybar-ben {
        gtk3 = libgtk3;
        haskellLib = haskell.lib;
        haskellPackages = self;
      })
        taffybar taffybar-ben;
    };
  };

in haskellPkgs
