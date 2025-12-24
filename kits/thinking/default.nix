{ config, lib, ... }:

with lib;
{
  options = {
    local.kits.thinking.enable = mkEnableOption "thinking";
  };
}

