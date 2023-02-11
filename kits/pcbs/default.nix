{ lib, ... }:

with lib;
{
  options = {
    local.kits.pcbs.enable = mkEnableOption "PCBs";
  };
}
