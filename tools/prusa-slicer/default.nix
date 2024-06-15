{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.kits._3d-printing;
in {
  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.prusa-slicer ];
  };
}
