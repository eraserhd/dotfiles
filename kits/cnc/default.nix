{ lib, ... }:

with lib;
{
  options = {
    local.kits.cnc.enable = mkEnableOption "CNC Head";
  };
}
