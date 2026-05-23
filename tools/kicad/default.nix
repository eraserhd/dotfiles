{ pkgs, lib, config, ... }:

with lib;
let
  cfg = config.local.kits.pcbs;
in {
  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.kicad ];
  };
}
