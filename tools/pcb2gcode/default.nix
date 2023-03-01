{ pkgs, lib, config, ... }:

with lib;
{
  config = mkIf config.local.kits.pcbs.enable {
    environment.systemPackages = [ pkgs.pcb2gcode ];
  };
}
