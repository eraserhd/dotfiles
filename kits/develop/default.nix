{ config, lib, ... }:

with lib;
{
  options = {
    local.kits.develop.enable = mkEnableOption "develop";
  };
}
