{ pkgs, lib, config, ... }:

with lib;
{
  config = mkIf config.local.kits.collaboration.enable {
    environment.systemPackages = [
      pkgs.zoom-us
    ];
  };
}
