{ lib, config, pkgs, ... }:

with lib;
let
  cfg = config.environment.xdgRuntimeDir;
  primaryUser = config.system.primaryUser;
  home = config.users.users.${primaryUser}.home;
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
      mkdir -p ${home}/.run
      chown ${primaryUser} ${home}/.run
      sudo -u ${primaryUser} launchctl setenv XDG_RUNTIME_DIR ${home}/.run
    '';
  };
}
