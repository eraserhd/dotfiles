{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.loginShell;
in {
  options = {
    local.loginShell.package = mkOption {
      type = types.package;
      default = pkgs.bashInteractive;
      description = ''
        Package containing user shell.
      '';
      example = "pkgs.bashInteractive";
    };
  };

  config = {
    users.defaultUserShell = cfg.package;
  };
}
