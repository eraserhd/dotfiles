{ lib, ... }:

with lib;
{
  options = {
    local.kits._3d-printing.enable = mkEnableOption "3D Printing";
  };
}
