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
    services.xserver = {
      enable = true;
      windowManager.i3.enable = true;
      autoRepeatDelay = 200;
      autoRepeatInterval = 40;
      libinput.mouse = {
        naturalScrolling = true;
        accelSpeed = "-0.5";
      };
    };
  }
  else {
  }));
}
