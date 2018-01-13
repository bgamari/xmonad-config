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

  environment.systemPackages = [
    #xmonad-ben
    haskellPackages.taffybar-ben
  ];
}
