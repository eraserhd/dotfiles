{ config, lib, ... }:

with lib;
{
  options = {
    local.kits.workstation.enable = mkEnableOption "workstation";
  };

  config = {
    plugbench.tokenFile = config.local.nats.clientTokenFile;

    plugbench.clipboard.enable = true;
    plugbench.kakoune.enable = true;
    plugbench.plumber.client = true;
  };
}
