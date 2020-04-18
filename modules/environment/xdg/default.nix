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
    environment.loginShellInit = ''
      mkdir -p "$HOME/.run"
    '';
    environment.variables = {
      XDG_RUNTIME_DIR = "\$HOME/.run";
    };
  };
}
