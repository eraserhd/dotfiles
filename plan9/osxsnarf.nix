{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.plan9;
in {
  config = mkIf (cfg.terminal.enable && pkgs.stdenv.isDarwin) {
    environment.systemPackages = [ pkgs.osxsnarf ];

    launchd.agents.osxsnarf = {
      command = "${pkgs.reattach-to-user-namespace}/bin/reattach-to-user-namespace " +
        "${pkgs.plan9port}/bin/9 ${pkgs.osxsnarf}/bin/osxsnarf -f " +
        "'unix!/Users/jfelice/.run/plan9/srv/snarf'";
      serviceConfig = {
        KeepAlive = true;
        UserName = "jfelice";
      };
    };
  };
}
