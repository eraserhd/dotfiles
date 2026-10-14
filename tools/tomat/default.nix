{ pkgs, lib, config, options, ... }:

with lib;
let
  isDarwin = builtins.hasAttr "launchd" options;
in {
  config = {
    environment.systemPackages = [ pkgs.tomat ];

    home-manager.users.jfelice = {
      services.tomat = {
        enable = true;

        settings = {
          timer = {
            work = 45;
            break = 15;
          };
        };
      };

      launchd.agents.tomat.config.EnvironmentVariables =
        optionalAttrs (isDarwin && config.environment.xdgRuntimeDir.enable) {
          XDG_RUNTIME_DIR = "${config.users.users.${config.system.primaryUser}.home}/.run";
        };
    };
  };
}
