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
      nixpkgs.config.allowUnfree = true;
      environment.systemPackages = [ pkgs.claude-code ];

      home-manager.users.jfelice.programs.claude-code = {
        enable = true;
        package = null; # Installed above.
        context = ./CLAUDE.md;
        # Merge into settings.json rather than owning it, as other settings
        # (work's) share the file and Claude Code writes to it.
        mutableSettings = true;
        settings = {
          theme = "dark";
          autoUpdatesChannel = "stable";
        };
      };
    }
    (mkIf cfg.enable {
      environment.systemPackages = [ pkgs.claude-desktop-fhs ];
    })
  ];
}
