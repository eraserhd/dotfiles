{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.local.plan9;
in {
  options = {
    local.plan9.terminal.enable = mkEnableOption "Plan9 terminal";
    local.plan9.cpu.enable = mkEnableOption "Plan9 CPU";
  };

  config = mkMerge [
    (mkIf (cfg.cpu.enable || cfg.terminal.enable) {
      environment.systemPackages = with pkgs; [
        plan9port osxsnarf
      ] ++ lib.optional pkgs.stdenv.isDarwin pkgs.osxfuse;
    })
    (mkIf cfg.terminal.enable {
      home-manager.users.jfelice = { pkgs, ... }: {
        home.file.".config/plan9/plumbing".source = ./plumbing;
      };
    })
  ];
}
