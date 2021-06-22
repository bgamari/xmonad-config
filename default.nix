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
    let rev = "7575a8b1fe6db99da6525563957514c19186fdc6";
    in fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
      sha256 = "sha256:1pc7lz2h5c4nkky5bf00hra443kk482gw3x09hzlf4s3178mq77f";
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
        rev = "6a7eb85e84ddc2091706fbce5ff293e859481e51";
        sha256 = "sha256:12lsa0008jij5p76f3g0b36bqjs72hpbln4yzfb330pz4zmkhxwq";
      }) { };

      X11 = callHackage "X11" "1.10" {};

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
