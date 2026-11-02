{ config, lib, pkgs, mattpocock-skills, ... }:

with lib;
let
  cfg = config.local.claudeDesktop;
  pocockSkill = path: "${mattpocock-skills}/skills/${path}";
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
          # Commands are written for bash's word splitting, not zsh's.
          env.CLAUDE_CODE_SHELL = "${pkgs.bashInteractive}/bin/bash";
        };
        skills = {
          grill-me = pocockSkill "productivity/grill-me";
          improve-codebase-architecture = pocockSkill "engineering/improve-codebase-architecture";

          # Called by the two above.
          codebase-design = pocockSkill "engineering/codebase-design";
          domain-modeling = pocockSkill "engineering/domain-modeling";
          grilling = pocockSkill "productivity/grilling";
        };
      };
    }
    (mkIf cfg.enable {
      environment.systemPackages = [ pkgs.claude-desktop-fhs ];
    })
  ];
}
