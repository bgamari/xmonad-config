{ config, pkgs, ... }:

let
  xmonad-ben = pkgs.callPackage (import ./.) {};
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
    #haskellPackages = pkgs.haskell.packages.ghc822;
    extraPackages = self:
      let 
        taffybar-ben = self.callCabal2nix "taffybar" ./taffybar.git { gtk2 = pkgs.gtk2; };
      in with self; [ errors dbus split data-default-instances-containers taffybar-ben ];
  };

  environment.systemPackages = [
    #xmonad-ben
  ];
}
