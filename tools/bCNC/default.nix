{ lib, config, pkgs, ... }:

with lib;
{
  config = mkIf config.local.kits.pcbs.enable {
    environment.systemPackages = [ pkgs.bCNC ];
  };
}
