{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.services.X11;
in {
  options = {
    local.services.X11.enable = mkEnableOption "X11";
  };

  config = mkIf cfg.enable ({
    environment.systemPackages = with pkgs; [
      xorg.xev
      flameshot
      playerctl
    ];
  } // (if (builtins.hasAttr "xserver" options.services)
  then {
    services.xserver = {
      enable = true;
      #windowManager.i3.enable = true;
      autoRepeatDelay = 200;
      autoRepeatInterval = 40;
    };
    services.libinput = {
      enable = true;
      mouse = {
        naturalScrolling = true;
        accelSpeed = "-0.9";
      };
    };
  }
  else {
  }));
}
