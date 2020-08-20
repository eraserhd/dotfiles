{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.services.X11;
in {
  options = {
    local.services.X11.enable = mkEnableOption "X11";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.xorg.xev
    ];
  };
}
