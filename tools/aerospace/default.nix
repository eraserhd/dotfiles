{ pkgs, options, lib, ... }:

with lib;
{
  # Tiling and workspaces only.  The 'C-w' prefix stays in Hammerspoon's
  # WindowSigils, which drives these via the 'aerospace' CLI, so almost nothing
  # is bound globally here.  See tools/hammerspoon/init.lua.
  config = (if (builtins.hasAttr "aerospace" options.services)
  then {
    # mkIf rather than folding the check into the 'if' above: gating the shape
    # of 'config' on 'pkgs' recurses, since 'pkgs' comes from config.nixpkgs.
    services.aerospace = mkIf pkgs.stdenv.hostPlatform.isAarch64 {
      enable = true;

      settings = {
        default-root-container-layout = "tiles";
        default-root-container-orientation = "horizontal";

        # i3-style splits, so WindowSigils' '-' and '\' keys mean what they say
        # instead of being normalized away.
        enable-normalization-flatten-containers = false;
        enable-normalization-opposite-orientation-for-nested-containers = false;

        # MouseFollowsFocus moves the pointer; don't fight it.  (The module
        # defaults this to [ "move-mouse monitor-lazy-center" ].)
        on-focused-monitor-changed = [ ];

        # Keep under WindowSigils' MINIMUM_EMPTY_SIZE of 20, or the gaps get
        # assigned sigils as if they were empty space.
        gaps = {
          inner.horizontal = 4;
          inner.vertical = 4;
          outer.left = 4;
          outer.right = 4;
          outer.top = 4;
          outer.bottom = 4;
        };

        # A way back in if Hammerspoon dies and takes 'C-w' with it.
        mode.main.binding = {
          alt-shift-semicolon = "mode service";
        };

        mode.service.binding = {
          esc = "mode main";
          r = [ "flatten-workspace-tree" "mode main" ];
          b = [ "balance-sizes" "mode main" ];
          f = [ "layout floating tiling" "mode main" ];
        };
      };
    };
  }
  else {
  });
}
