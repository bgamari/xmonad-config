{ stdenv, fetchgit, libdbusmenu-gtk3, dbus, gtk3, haskell, haskellPackages }:

let
  inherit (haskell) callHackage callCabal2nix;
  inherit (haskell.lib) enableDWARFDebugging enableExecutableProfiling addPkgconfigDepend;

  haskellPkgs = haskellPackages.override {
    overrides = self: super: rec {
      # Xmonad
      xmonad = self.callCabal2nix "xmonad" (fetchgit { url = "git://github.com/xmonad/xmonad"; rev = "ecf1a0ca0d094c76a18c2c1b77ac7c1dcac10f5e"; sha256 = null; }) {};
      xmonad-contrib = self.callCabal2nix "xmonad-contrib" ./xmonad-contrib {};

      # Taffybar and dependencies
      taffybar        = enableDWARFDebugging (addPkgconfigDepend (self.callCabal2nix "taffybar" ./taffybar-new {}) gtk3);
      dbus            = self.callHackage "dbus" "1.0.1" {};
      dbus-hslogger   = self.callHackage "dbus-hslogger" "0.1.0.1" { inherit dbus; };
      gtk-sni-tray    = addPkgconfigDepend (self.callHackage "gtk-sni-tray" "0.1.3.1" {}) gtk3;
      gtk-strut       = self.callHackage "gtk-strut" "0.1.2.1" {};
      status-notifier-item = self.callHackage "status-notifier-item" "0.2.2.0" { };
      gi-dbusmenugtk3 = addPkgconfigDepend (self.callHackage "gi-dbusmenugtk3" "0.4.2" {
        dbusmenu-gtk3 = libdbusmenu-gtk3;
      }) gtk3;
      
      # To satisfy xmonad-with-packages
      hint = self.callHackage "hint" "0.8.0" {};
      exceptions = self.callHackage "exceptions" "0.10.0" {};

      # N.B. taffybar segfaults without profiling
      taffybar-ben = enableExecutableProfiling (enableDWARFDebugging (self.callCabal2nix "taffybar-ben" ./taffybar-ben {}));
      xmonad-ben = self.callCabal2nix "xmonad-ben" ./xmonad-ben {};
    };
  };

in haskellPkgs
