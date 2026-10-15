{ lib, config, pkgs, ... }:

with lib;
{
  config = mkIf config.local.kits.cnc.enable {
    home-manager.users.jfelice.home.packages = [ pkgs.minicom ];
  };
}
