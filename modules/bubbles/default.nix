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
  }
  else {
  });
}
