{ config, lib, ... }:

with lib;
let
  cfg = config.services.hammerspoon;
in {
  options.services.hammerspoon = {
    enable = mkEnableOption "Hammerspoon";
  };

  config = mkIf cfg.enable {
    homebrew.casks = [ "hammerspoon" ];
  };
}
