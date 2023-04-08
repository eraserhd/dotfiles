{ pkgs, lib, config, ... }:

with lib;
let
  cfg = config.local.kits._3d-printing;
in {
  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.inkscape ];
  };
}
