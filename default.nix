{ stdenv, fetchurl, fetchgit, fetchFromGitHub, libdbusmenu-glib, libdbusmenu-gtk3, dbus, glib, gtk3, cairo, haskell, haskellPackages }:

let
  inherit (haskell) callHackage callCabal2nix;
  inherit (haskell.lib) enableDWARFDebugging enableExecutableProfiling
                        addPkgconfigDepend dontCheck doJailbreak;

  libglib = glib;
  libgtk3 = gtk3;
  libcairo = cairo;

  gtk2hs-src = fetchFromGitHub {
    owner = "gtk2hs";
    repo = "gtk2hs";
    rev = "7bccd432e2f962d80b2b804fa2a59712e402753c";
    sha256 = "0jxl55ywancz6wxvzx513bywi5gqyn1zk2l0vp707r08ygprrjij";
  };

  haskellPkgs = haskell.packages.ghc861.override {
    all-cabal-hashes = 
      let rev = "2a3b5454330d92680607f500322c73cc7e730058";
      in fetchurl {
        url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
        sha256 = "04l4k2qjglgwvx9hypbm1vgrjhiw3s2alm2xllin4iax5qgpvdq0";
      };

    overrides = self: super: rec {
      # Xmonad
      xmonad = (self.callCabal2nix "xmonad" (fetchFromGitHub {
        name = "xmonad";
        owner = "xmonad";
        repo = "xmonad";
        rev = "bb13853929f8f6fc59b526bcc10631e1bac309ad";
        sha256 = "1f2w0vkv4i40sa52d0bxdhmn9zsikzymm91xwdi4m64nqwip1i97";
      }) {}).overrideAttrs (old: { postInstall = ""; });

      xmonad-contrib = self.callCabal2nix "xmonad-contrib" ./xmonad-contrib {};

      # Taffybar and dependencies
      taffybar             = enableDWARFDebugging (self.callCabal2nix "taffybar" ./taffybar-new { gtk3 = libgtk3; });
      hslogger             = self.callHackage "hslogger" "1.2.12" {};
      HTTP                 = self.callHackage "HTTP" "4000.3.12" {};
      rate-limit           = self.callHackage "rate-limit" "1.4.1" {};
      gtk3                 = self.callHackage "gtk3"       "0.15.0" { gtk3 = libgtk3; };
      gtk2hs-buildtools    = self.callCabal2nix "gtk2hs-buildtools" "${gtk2hs-src}/tools" {};
      glib                 = self.callCabal2nix "glib" "${gtk2hs-src}/glib" { glib = libglib; };
      cairo                = self.callCabal2nix "cairo" "${gtk2hs-src}/cairo" { cairo = libcairo; };
      gtk-sni-tray         = self.callHackage "gtk-sni-tray" "0.1.5.0" { gtk3 = libgtk3; };
      gtk-strut            = self.callHackage "gtk-strut" "0.1.3.0" {};
      #status-notifier-item = self.callHackage "status-notifier-item" "0.3.0.0" { };
      gi-dbusmenu          = self.callHackage "gi-dbusmenu" "0.4.1" { };

      # To satisfy xmonad-with-packages
      hint = self.callHackage "hint" "0.9.0" {};

      # N.B. taffybar segfaults without profiling
      taffybar-ben = enableExecutableProfiling (enableDWARFDebugging (self.callCabal2nix "taffybar-ben" ./taffybar-ben {}));
      xmonad-ben = self.callCabal2nix "xmonad-ben" ./xmonad-ben {};
    };
  };

in haskellPkgs
