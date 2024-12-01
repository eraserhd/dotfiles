{ pkgs, config, lib, ... }:

with lib;
let
  isWorkstation = config.local.kits.workstation.enable;
in {
  config = mkIf isWorkstation {
    environment.systemPacakges = [ pkgs.obsidian ];
  };
}
