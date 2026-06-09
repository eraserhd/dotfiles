{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.claudeDesktop;
in {
  options = {
    local.claudeDesktop.enable = mkEnableOption "Claude Desktop";
  };

  config = mkMerge [
    {
      environment.systemPackages = [ pkgs.claude-code ];
    }
    (mkIf cfg.enable {
      environment.systemPackages = [ pkgs.claude-desktop-fhs ];
    })
  ];
}
