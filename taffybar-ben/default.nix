{ gtk3, haskellLib, haskellPackages }:

let 
  inherit (haskellLib) enableDWARFDebugging enableExecutableProfiling
                       addPkgconfigDepend dontCheck doJailbreak addBuildDepends;
in
rec {
  taffybar = enableDWARFDebugging (haskellPackages.callCabal2nix "taffybar" ../taffybar-new {
    inherit gtk3;
    dbus = haskellPackages.dbus;
  });
  hslogger = haskellPackages.callHackage "hslogger" "1.2.12" {};
  taffybar-ben = enableDWARFDebugging (haskellPackages.callCabal2nix "taffybar-ben" ./. {});
}
