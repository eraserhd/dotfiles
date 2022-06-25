{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.services.X11;
in {
  options = {
    local.services.X11.enable = mkEnableOption "X11";
  };

  config = mkIf cfg.enable ({
    environment.systemPackages = [
      pkgs.xorg.xev
    ];
  } // (if (builtins.hasAttr "xserver" options.services)
  then {
    services.xserver.enable = true;
    services.xserver.windowManager.i3.enable = true;
    services.xserver.displayManager.sessionCommands = ''
      ${pkgs.xorg.xset}/bin/xset r rate 200 60
    '';
  }
  else {
  }));
}
