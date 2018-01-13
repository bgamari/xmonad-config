{ config, pkgs, ... }:

let
  xmonad-ben = pkgs.callPackage (import ./.) {};
  haskellPackages = pkgs.haskell.packages.ghc802.override {
    overrides = self: super: {
      taffybar = self.callCabal2nix "taffybar" ./taffybar.git { gtk2 = pkgs.gtk2; };
      taffybar-ben = self.callCabal2nix "taffybar-ben" ./taffybar {};
    };
  };
in {
  services.xserver.windowManager = {
    session = [{
      name = "xmonad-ben";
      start = ''
        ${xmonad-ben}/start.sh &
        waitPID=$!
      '';
    }];
  };

  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    inherit haskellPackages;
    extraPackages = self:
      with self; [ errors dbus split data-default-instances-containers ];
  };

  systemd.user.services.taffybar = {
    description = "Taffybar";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = "${haskellPackages.taffybar-ben}/bin/taffybar";
      RestartSec = 3;
      Restart = "always";
    };
  };

  systemd.user.services.gnome-settings-daemon = {
    description = "gnome-settings-daemon";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.gnome3.gnome_settings_daemon}/libexec/gsd-xsettings";
      RestartSec = 3;
      Restart = "always";
    };
  };
}
