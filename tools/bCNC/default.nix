{ lib, config, pkgs, ... }:

with lib;
{
  config = mkIf config.local.kits.cnc.enable {
    #FIXME:
    #environment.systemPackages = [ pkgs.bCNC ];
  };
}
