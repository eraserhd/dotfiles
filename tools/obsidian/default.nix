{ pkgs, config, lib, ... }:

with lib;
{
  config = mkIf config.local.kits.thinking.enable {
    environment.systemPackages = [ pkgs.obsidian ];
  };
}
