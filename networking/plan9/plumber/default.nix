{ config, lib, options, pkgs, ... }:

with lib;
let
  cfg = config.local.plan9.terminal;

  plumbing = pkgs.runCommand "plumbing" {} ''
    substitute ${./plumbing} $out \
      --subst-var-by nexus_tools '${pkgs.nexus-tools}'
  '';

in {
  config = mkIf cfg.enable (if (builtins.hasAttr "launchd" options)
  then {
    launchd.user.agents.plumber = {
      script = ''
        exec bash -l -c '
          ${pkgs.reattach-to-user-namespace}/bin/reattach-to-user-namespace \
              ${pkgs.plan9port-wrapper}/bin/9 plumber -p ${plumbing} -f
        '
      '';
      serviceConfig = {
        KeepAlive = true;
      };
    };
  }
  else {
  });
}
