{ config, pkgs, ... }:

let
  #nixpkgs = import ./nixpkgs.nix {};
  nixpkgs = import <nixpkgs> {};
  haskellPackages = nixpkgs.callPackage (import ./default.nix) {
    haskellPackages = nixpkgs.haskell.packages.ghc882;
  };

in {
  environment.systemPackages = with pkgs; [
    neovim-qt rofi gnome3.gnome_session xorg.xmessage mattermost-desktop
  ];

  services.gnome3.gnome-keyring.enable = true;
  #services.arbtt.enable = true;
  services.arbtt.package = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.arbtt;

  services.geoclue2.enable = true;
  location = {
    provider = "manual";
    latitude  = 43.0755;
    longitude = -70.760;
  };
  services.redshift = {
    enable = true;
  };

  services.compton = {
    enable = true;
    backend = "glx";
    #vSync = true;
    #extraOptions = ''
    #  # Otherwise emacs fails to redraw
    #  xrender-sync = true;
    #  paint-on-overlay = true;
    #'';
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
      template = {description, script, enable ? true, requires ? [], environment ? {}, type ? "simple"} : {
        inherit description script enable requires environment;
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];
        serviceConfig = {
          Type = type;
          RestartSec = 3;
          Restart = "always";
        };
      };
    in {
      taffybar = template {
        description = "Taffybar";
        script = "${haskellPackages.taffybar-ben}/bin/taffybar-ben";
        requires = [ "status-notifier-watcher.service" ];
        environment = {
          # otherwise it crashes due to missing icons
          XDG_DATA_DIRS = "/run/current-system/sw/share";
        };
      };

      status-notifier-watcher = template {
        #enable = false;
        description = "Status notifier watcher";
        script = "${haskellPackages.status-notifier-item}/bin/status-notifier-watcher";
      };

      # For HexChat, blueman, et al.
      xembed-sni-proxy = template {
        enable = false; # inexplicably doesn't start under systemd
        description = "XEmbed SNI proxy";
        script = "${pkgs.strace}/bin/strace ${pkgs.plasma5.plasma-workspace}/bin/xembedsniproxy";
        environment = {
          # otherwise it crashes due to missing icons
          XDG_DATA_DIRS = "/run/current-system/sw/share";
        };
      };

      gnome-settings-daemon = template {
        enable = false;
        description = "gnome-settings-daemon";
        script = "${pkgs.gnome3.gnome_settings_daemon}/libexec/gsd-xsettings";
      };

      blueman-applet = template {
        enable = false;
        description = "blueman";
        script = "${pkgs.blueman}/bin/blueman-applet --indicator";
      };

      nm-applet = template {
        description = "network-manager applet";
        script = "${pkgs.networkmanagerapplet}/bin/nm-applet --indicator --sm-disable";
      };

      printer-applet = template {
        description = "printer applet";
        script = "${pkgs.system-config-printer}/bin/system-config-printer-applet";
      };

      dunst = template {
        description = "dunst notification daemon";
        script = "${pkgs.dunst}/bin/dunst -conf ${./dunstrc}";
      };

      set-background = template {
        description = "set background color";
        script = "xsetroot -solid midnightblue";
        type = "oneshot";
      };
    };
}
