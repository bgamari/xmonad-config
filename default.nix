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

  gtk2hs-src = fetchFromGitHub {
    owner = "gtk2hs";
    repo = "gtk2hs";
    rev = "7bccd432e2f962d80b2b804fa2a59712e402753c";
    sha256 = "0jxl55ywancz6wxvzx513bywi5gqyn1zk2l0vp707r08ygprrjij";
  };

  all-cabal-hashes = 
    let rev = "ee621f2f8401141df90c62a3d18a686e3efd7407";
    in fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
      sha256 = "1474sy5kzxv78mkkfnzsxp5is8xcv0ijqyxc6633am7471mr50mw";
    };

  haskellPkgs = haskellPackages.override {
    inherit all-cabal-hashes;

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
      taffybar             = enableDWARFDebugging (self.callCabal2nix "taffybar" ./taffybar-new { gtk3 = libgtk3; dbus = self.dbus; });
      #taffybar             = self.callHackage "taffybar" "3.2.0" { gtk3 = libgtk3; dbus = self.dbus; };
      hslogger             = self.callHackage "hslogger" "1.2.12" {};
      #HTTP                 = self.callHackage "HTTP" "4000.3.12" {};
      #rate-limit           = self.callHackage "rate-limit" "1.4.1" {};
      #network              = dontCheck (self.callHackage "network" "3.0.1.0" {});
      #network-bsd          = self.callHackage "network-bsd" "2.8.1.0" {};
      #gtk3                 = self.callHackage "gtk3"       "0.15.0" { gtk3 = libgtk3; };
      #gtk2hs-buildtools    = self.callCabal2nix "gtk2hs-buildtools" "${gtk2hs-src}/tools" {};
      #glib                 = self.callCabal2nix "glib" "${gtk2hs-src}/glib" { glib = libglib; };
      #cairo                = self.callCabal2nix "cairo" "${gtk2hs-src}/cairo" { cairo = libcairo; };
      #gtk-sni-tray         = self.callHackage "gtk-sni-tray" "0.1.5.0" { gtk3 = libgtk3; };
      #gtk-strut            = self.callHackage "gtk-strut" "0.1.3.0" {};
      #dbus                 = self.callHackage "dbus" "1.2.4" {};
      #status-notifier-item = self.callHackage "status-notifier-item" "0.3.0.1" { };
      #gi-gdkpixbuf         = self.callHackage "gi-gdkpixbuf" "2.0.18" { };

      haskell-gi            = self.callHackage "haskell-gi" "0.23.0" { };
      haskell-gi-base       = self.callHackage "haskell-gi-base" "0.23.0" { inherit glib; };
      gi-cairo-connector   = self.callHackage "gi-cairo-connector" "0.0.1" { };
      gi-cairo-render      = self.callHackage "gi-cairo-render" "0.0.1" { cairo = libcairo; };
      gi-cairo             = addBuildDepends (self.callHackage "gi-cairo" "1.0.23" { cairo = libcairo; }) [ gobject-introspection ];
      gi-dbusmenu          = self.callHackage "gi-dbusmenu" "0.4.7" { };
      gi-dbusmenugtk3      = self.callHackage "gi-dbusmenugtk3" "0.4.8" { };
      gi-atk               = self.callHackage "gi-atk" "2.0.21" { };
      gi-gio               = self.callHackage "gi-gio" "2.0.25" { };
      gi-gdkpixbuf         = addBuildDepends (self.callHackage "gi-gdkpixbuf" "2.0.23" { }) [ gtk3 ];
      gi-gdkx11            = addBuildDepends (self.callHackage "gi-gdkx11" "3.0.9" { }) [ gtk3 ];
      gi-gdk               = self.callHackage "gi-gdk" "3.0.22" { };
      gi-gtk               = self.callHackage "gi-gtk" "3.0.32" { };
      gi-gtk-hs            = self.callHackage "gi-gtk-hs" "0.3.8.0" { };
      gi-pango             = self.callHackage "gi-pango" "1.0.22" { cairo = cairo; pango = pango; };
      gi-xlib              = addBuildDepends (self.callHackage "gi-xlib" "2.0.8" { }) [ xorg.libX11 gobject-introspection ];
      gi-glib              = addBuildDepends (self.callHackage "gi-glib" "2.0.23" { glib = libglib; }) [ gobject-introspection ];
      gi-gobject           = addBuildDepends (self.callHackage "gi-gobject" "2.0.22" { glib = libglib; }) [ gobject-introspection ];

      #socks                = self.callCabal2nix "socks" ./hs-socks { };
      #simple-sendfile      = self.callHackage "simple-sendfile" "0.2.28" { };
      #warp                 = dontCheck (self.callHackage "warp" "3.2.26" { });
      #OneTuple             = self.callHackage "OneTuple" "0.2.2" { };
      broadcast-chan       = doJailbreak (self.callHackage "broadcast-chan" "0.2.0.2" { });
      #entropy              = self.callHackage "OneTuple" "0.2.2" { };

      # To satisfy xmonad-with-packages
      #hint = self.callHackage "hint" "0.9.0" {};

      # N.B. taffybar segfaults without profiling
      taffybar-ben = enableExecutableProfiling (enableDWARFDebugging (self.callCabal2nix "taffybar-ben" ./taffybar-ben {}));
      xmonad-ben = self.callCabal2nix "xmonad-ben" ./xmonad-ben {};
      
      hpc-coveralls = doJailbreak super.hpc-coveralls;
    };
  };

in haskellPkgs
