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
      xev
      flameshot
      playerctl
    ];

    # flameshot otherwise screenshots through xdg-desktop-portal, which
    # nothing provides under a bare window manager.
    home-manager.users.jfelice.xdg.configFile."flameshot/flameshot.ini".text =
      generators.toINI {} {
        General = {
          useX11LegacyScreenshot = true;
          savePath = config.users.users.jfelice.home;
        };
      };
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
