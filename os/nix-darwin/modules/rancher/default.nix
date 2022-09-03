{ lib, config, ... }:

with lib;
let
  cfg = config.local.rancherDesktop;
in {
  options = {
    local.rancherDesktop.enable = mkEnableOption "Rancher";
  };

  config = mkIf cfg.enable {
    homebrew.casks = [ "rancher" ];
    environment.systemPath = [ "~/.rd/bin" ];
  };
}
