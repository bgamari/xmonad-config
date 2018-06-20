{ config, pkgs, ... }:

let
  haskellPackages =
    let pkgs = import /home/ben/.nix-overlay/nixpkgs {};
    in pkgs.callPackage (import ./default.nix) { haskellPackages = pkgs.haskell.packages.ghc843;};

in {
  environment.systemPackages = with pkgs; [ gmrun gnome3.gnome_session ];

  services.gnome3.gnome-keyring.enable = true;
  services.arbtt.enable = true;

  services.compton = {
    enable = true;
    backend = "glx";
    vSync = "opengl";
    extraOptions = ''
      # Otherwise emacs fails to redraw
      #xrender-sync = true;
      #paint-on-overlay = true;
    '';
  };

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
      template = {description, cmd, enable ? true, requires ? []} : {
        inherit description enable requires;
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
        requires = [ "status-notifier-watcher.service" ];
      };

      status-notifier-watcher = template {
        description = "Status notifier watcher";
        cmd = "${haskellPackages.status-notifier-item}/bin/status-notifier-watcher";
      };

      # For HexChat, blueman, et al.
      xembed-sni-proxy = template {
        description = "XEmbed SNI proxy";
        cmd = "${pkgs.plasma5.plasma-workspace}/bin/xembedsniproxy";
      };

      gnome-settings-daemon = template {
        enable = false;
        description = "gnome-settings-daemon";
        cmd = "${pkgs.gnome3.gnome_settings_daemon}/libexec/gsd-xsettings";
      };

      blueman-applet = template {
        description = "blueman";
        cmd = "${pkgs.blueman}/bin/blueman-applet --indicator";
      };

      nm-applet = template {
        description = "network-manager applet";
        cmd = "${pkgs.networkmanagerapplet}/bin/nm-applet --indicator --sm-disable";
      };

      printer-applet = template {
        description = "printer applet";
        cmd = "${pkgs.system-config-printer}/bin/system-config-printer-applet --indicator";
      };
    };
}
