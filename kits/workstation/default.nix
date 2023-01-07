{ lib, ... }:

with lib;
{
  options = {
    local.kits.workstation.enable = mkEnableOption "workstation";
  };
}

