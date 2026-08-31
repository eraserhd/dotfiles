{ lib, config, pkgs, ... }:

with lib;
let
  cfg = config.environment.xdgRuntimeDir;
in {
  options = {
    environment.xdgRuntimeDir.enable = mkEnableOption "Set and make per-user $XDG_RUNTIME_DIR";
  };
  config = mkIf cfg.enable {
    assertions = [{
      assertion = pkgs.stdenv.isDarwin;
      message = "environment.xdgRuntimeDir.enable is only useful on Darwin";
    }];
    environment.variables.XDG_RUNTIME_DIR = "\$HOME/.run";
    system.activationScripts.userLaunchd.text = ''
      mkdir -p /Users/${config.system.primaryUser}/.run
      chown ${config.system.primaryUser} /Users/${config.system.primaryUser}/.run
      sudo -u ${config.system.primaryUser} launchctl setenv XDG_RUNTIME_DIR /Users/${config.system.primaryUser}/.run
    '';
  };
}
