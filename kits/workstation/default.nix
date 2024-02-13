{ config, lib, ... }:

with lib;
let
  cfg = config.local.kits.workstation;
in {
  options = {
    local.kits.workstation.enable = mkEnableOption "workstation";
  };

  config = mkIf cfg.enable {
    plugbench.token = config.services.nats.settings.authorization.token;

    plugbench.clipboard.enable = true;
    plugbench.kakoune.enable = true;
  };
}

