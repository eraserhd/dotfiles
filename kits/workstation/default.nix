{ config, lib, ... }:

with lib;
let
  cfg = config.local.kits.workstation;
in {
  options = {
    local.kits.workstation.enable = mkEnableOption "workstation";
  };

  config = mkIf cfg.enable {
    plugbench.kakoune.enable = true;
  };
}

