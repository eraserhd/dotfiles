{ lib, config, pkgs, ... }:

with lib;
{
  config = mkIf config.local.kits.cnc.enable {
    environment.systemPackages = [ pkgs.bCNC ];
  };
}
