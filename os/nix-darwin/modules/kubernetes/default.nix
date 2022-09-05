{ lib, config, ... }:

with lib;
let
  cfg = config.services.k3s;
in {
  options = {
    services.k3s.enable = mkEnableOption "k3s via Rancher Desktop";
  };

  config = mkIf cfg.enable {
    homebrew.casks = [ "rancher" ];

    environment.systemPath = [ "~/.rd/bin" ];
  };
}
