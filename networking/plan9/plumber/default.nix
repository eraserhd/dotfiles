{ config, lib, options, pkgs, ... }:

with lib;
let
  cfg = config.local.plan9.terminal;
in {
  config = mkIf cfg.enable (if (builtins.hasAttr "launchd" options)
  then {
    launchd.user.agents.plumber = {
      script = ''
        exec zsh -l -c '
          ${pkgs.reattach-to-user-namespace}/bin/reattach-to-user-namespace \
              ${pkgs.plan9port-wrapper}/bin/9 plumber -p ${./plumbing} -f
        '
      '';
      serviceConfig = {
        KeepAlive = true;
        StandardOutPath = "/tmp/out2.log";
        StandardErrorPath = "/tmp/err2.log";
      };
    };
  }
  else {
  });
}
