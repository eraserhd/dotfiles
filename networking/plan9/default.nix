{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.local.plan9;
in {
  imports = [
    ./osxsnarf.nix
  ];

  options = {
    local.plan9.terminal.enable = mkEnableOption "Plan9 terminal";
    local.plan9.cpu.enable = mkEnableOption "Plan9 CPU";
  };

  config = mkIf (cfg.cpu.enable || cfg.terminal.enable) {
    environment.systemPackages = [ pkgs.plan9port ]
      ++ optional pkgs.stdenv.isDarwin pkgs.osxfuse
      ++ optional (!pkgs.stdenv.isDarwin) pkgs.fusePackages.fuse_3;

    local.browserCommand = "${pkgs.plan9port}/bin/9 plumb";

    programs.bash.interactiveShellInit = ''
      if [[ -z $XDG_RUNTIME_DIR ]]; then
          export XDG_RUNTIME_DIR=~/.run
      fi
      if [[ -z $NAMESPACE ]]; then
          export NAMESPACE=$XDG_RUNTIME_DIR/plan9/srv
      fi
      mkdir -p $NAMESPACE $XDG_RUNTIME_DIR/plan9/log
    '';
  };
}
