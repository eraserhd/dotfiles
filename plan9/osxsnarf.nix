{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.plan9;
in {
  config = mkIf (cfg.terminal.enable && pkgs.stdenv.isDarwin) {
    environment.systemPackages = [ pkgs.osxsnarf ];
  };
}
