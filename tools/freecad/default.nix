{ lib, config, pkgs, ... }:

with lib;
let
  cfg = config.local.kits._3d-printing;
in {
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ freecad ];
  };
}
