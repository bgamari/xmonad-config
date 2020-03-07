{ stdenv, fetchurl, fetchgit, fetchFromGitHub,
  libdbusmenu-glib, libdbusmenu-gtk3, dbus, glib, gtk3, cairo, pango, 
  gobject-introspection,
  haskell, haskellPackages, xorg
}:

let
  inherit (haskell) callHackage callCabal2nix;
  inherit (haskell.lib) enableDWARFDebugging enableExecutableProfiling
                        addPkgconfigDepend dontCheck doJailbreak addBuildDepends;

  libglib = glib;
  libgtk3 = gtk3;
  libcairo = cairo;

  all-cabal-hashes = 
    let rev = "ee621f2f8401141df90c62a3d18a686e3efd7407";
    in fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
      sha256 = "1474sy5kzxv78mkkfnzsxp5is8xcv0ijqyxc6633am7471mr50mw";
    };

  haskellPkgs = haskell.packages.ghc883.override {
    #inherit all-cabal-hashes;

    overrides = self: super: rec {
      # Xmonad
      xmonad = self.callCabal2nix "xmonad" (fetchFromGitHub {
        name = "xmonad";
        owner = "xmonad";
        repo = "xmonad";
        rev = "bb13853929f8f6fc59b526bcc10631e1bac309ad";
        sha256 = "1f2w0vkv4i40sa52d0bxdhmn9zsikzymm91xwdi4m64nqwip1i97";
      }) {};

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
