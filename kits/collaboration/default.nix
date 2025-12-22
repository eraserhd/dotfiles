{ config, lib, ... }:

with lib;
{
  options = {
    local.kits.collaboration.enable = mkEnableOption "collaboration";
  };
}
