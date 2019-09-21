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

  config = mkMerge [
    (mkIf (cfg.cpu.enable || cfg.terminal.enable) {
      environment.systemPackages = (with pkgs; [
        plan9port
      ]) ++ optionals pkgs.stdenv.isDarwin (with pkgs; [
        osxfuse
      ]);

      programs.bash.interactiveShellInit = ''
        if [[ -z $XDG_RUNTIME_DIR ]]; then
            export XDG_RUNTIME_DIR=~/.run
        fi
        if [[ -z $NAMESPACE ]]; then
            export NAMESPACE=$XDG_RUNTIME_DIR/plan9/srv
        fi
        mkdir -p $NAMESPACE $XDG_RUNTIME_DIR/plan9/log
      '';
    })
    (mkIf cfg.terminal.enable {
      home-manager.users.jfelice = { pkgs, ... }: {
        home.file.".config/plan9/plumbing".source = ./plumbing;
      };
    })
  ];
}
