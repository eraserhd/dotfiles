{ config, lib, ... }:

with lib;
{
  options = {
    local.kits.workstation.enable = mkEnableOption "workstation";
  };

  config = {
    plugbench.token = config.services.nats.settings.authorization.token;

    plugbench.clipboard.enable = true;
    plugbench.kakoune.enable = true;
    plugbench.plumber.client = true;
  };
}

