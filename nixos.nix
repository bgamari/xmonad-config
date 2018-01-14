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
  environment.systemPackages = with pkgs; [ gmrun ];

  services.arbtt.enable = true;
  services.compton.enable = true;
  services.gnome3.gnome-keyring.enable = true;

  # 17 June 2016: Use Xinput2 for drag scrolling
  environment.variables.MOZ_USE_XINPUT2 = "1";

  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    inherit haskellPackages;
    extraPackages = self:
      with self; [ errors dbus split data-default-instances-containers ];
  };

  systemd.user.services =
    let
      template = {description, cmd, enable ? true} : {
        inherit description enable;
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];
        serviceConfig = {
          ExecStart = cmd;
          RestartSec = 3;
          Restart = "always";
        };
      };
    in {
      taffybar = template {
        description = "Taffybar";
        cmd = "${haskellPackages.taffybar-ben}/bin/taffybar-ben";
      };

      gnome-settings-daemon = template {
        enable = false;
        description = "gnome-settings-daemon";
        cmd = "${pkgs.gnome3.gnome_settings_daemon}/libexec/gsd-xsettings";
      };

      blueman-applet = template {
        description = "blueman";
        cmd = "${pkgs.blueman}/bin/blueman-applet";
      };

      nm-applet = template {
        description = "network-manager applet";
        cmd = "${pkgs.networkmanagerapplet}/bin/nm-applet";
      };

      printer-applet = template {
        description = "printer applet";
        cmd = "${pkgs.networkmanagerapplet}/bin/nm-applet";
      };
    };
}
