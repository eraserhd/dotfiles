{ lib, config, ... }:

with lib;
let
  cfg = config.local.kubernetes;
in {
  options = {
    local.kubernetes.enable = mkEnableOption "Rancher";
  };

  config = mkIf cfg.enable {
    homebrew.casks = [ "rancher" ];
    environment.systemPath = [ "~/.rd/bin" ];
  };
}
