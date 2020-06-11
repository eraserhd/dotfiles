{ options, config, lib, ... }:

with lib;
let
  cfg = config.bubbles;
in {
  options = {
    bubbles.enable = mkEnableOption "Bubbles";
  };
  config = mkIf cfg.enable (if (builtins.hasAttr "defaults" options.system)
  then {
    system.defaults.finder.CreateDesktop = false;
    system.defaults.dock.autohide = true;
    system.defaults.NSGlobalDomain._HIHideMenuBar = true;
  }
  else {
  });
}
