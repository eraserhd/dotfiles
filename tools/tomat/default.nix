{ pkgs, lib, config, options, ... }:

with lib;
let
  isDarwin = builtins.hasAttr "launchd" options;

  tomat = pkgs.tomat.overrideAttrs (old: {
    # plutil isn't available in the build sandbox.
    checkFlags = (old.checkFlags or []) ++ [ "--skip=service::tests::launchd_service_is_a_valid_plist" ];
  });
in {
  config = {
    environment.systemPackages = [ tomat ];

    home-manager.users.jfelice = {
      services.tomat = {
        enable = true;
        package = tomat;

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
