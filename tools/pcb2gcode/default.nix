{ pkgs, lib, config, ... }:

with lib;
{
  config = mkIf config.local.kits.pcbs.enable {
  # FIXME:
  #  environment.systemPackages = [ pkgs.pcb2gcode ];
  };
}
