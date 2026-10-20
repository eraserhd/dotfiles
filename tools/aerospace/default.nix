{ config, pkgs, options, lib, ... }:

with lib;
let
  aerospace = "${config.services.aerospace.package}/bin/aerospace";
  sigils = "${pkgs.local.sigils}/bin/sigils";

  # The letters tools/xmonad/config/xmonad.hs uses, then ';' and '/'.  None of
  # the keys bound in the sigils mode below can be one.
  sigilPool = "abcdegimnopqrstuvwxyz;/";
  keyNames = { ";" = "semicolon"; "/" = "slash"; };
  sigilKeys = map (c: { char = c; key = keyNames.${c} or c; }) (stringToCharacters sigilPool);

  # An empty space's sigil does nothing.
  focusSigil = pkgs.writeShellApplication {
    name = "focus-sigil";
    text = ''
      id=$(${sigils} "$1")
      [ "$id" != 0 ] && exec ${aerospace} focus --window-id "$id"
    '';
  };

  # Like XMonad's greedyView: a workspace showing on another monitor swaps with
  # the focused monitor's, where 'summon-workspace' alone sends ours somewhere
  # arbitrary.
  swapInWorkspace = pkgs.writeShellApplication {
    name = "swap-in-workspace";
    text = ''
      workspace=$1
      current_workspace=$(${aerospace} list-workspaces --focused)
      current_monitor=$(${aerospace} list-monitors --focused --format '%{monitor-id}')
      other_monitor=
      while read -r name monitor; do
        if [ "$name" = "$workspace" ] && [ "$monitor" != "$current_monitor" ]; then
          other_monitor=$monitor
        fi
      done < <(${aerospace} list-workspaces --monitor all --visible --format '%{workspace} %{monitor-id}')

      ${aerospace} summon-workspace "$workspace"
      if [ -n "$other_monitor" ]; then
        ${aerospace} focus-monitor "$other_monitor"
        ${aerospace} summon-workspace "$current_workspace"
        ${aerospace} focus-monitor "$current_monitor"
      fi
    '';
  };

  rerunLastCommand = "/run/current-system/sw/bin/kitty @ --to unix:${config.users.users.jfelice.home}/.run/kitty send-text --match=title:kak_repl_window '\\x10\\x0d'";

  directions = { h = "left"; j = "down"; k = "up"; l = "right"; };
  workspaces = genAttrs (map toString (range 0 9)) (d: if d == "0" then "10" else d);

  # Each key leaves the mode, as WindowSigils' did.
  sigilsMode = listToAttrs (map ({ char, key }: nameValuePair key [
      "mode main"
      "exec-and-forget ${focusSigil}/bin/focus-sigil '${char}'"
    ]) sigilKeys)
    // mapAttrs' (key: direction: nameValuePair key [ "mode main" "focus ${direction}" ]) directions
    // mapAttrs' (key: direction: nameValuePair "alt-${key}" [
      "mode main" "swap ${direction}" "move-mouse window-lazy-center"
    ]) directions
    // mapAttrs' (key: direction: nameValuePair "ctrl-${key}" [
      "mode main" "join-with ${direction}" "move-mouse window-lazy-center"
    ]) directions
    // mapAttrs' (key: workspace: nameValuePair key [
      "mode main" "exec-and-forget ${swapInWorkspace}/bin/swap-in-workspace ${workspace}"
    ]) workspaces
    // mapAttrs' (key: workspace: nameValuePair "ctrl-${key}" [
      "mode main" "move-node-to-workspace ${workspace}"
    ]) workspaces
    // {
      esc = "mode main";
      f = [ "mode main" "fullscreen" ];
      equal = [ "mode main" "balance-sizes" ];
      minus = [ "mode main" "layout v_tiles" "move-mouse window-lazy-center" ];
      shift-backslash = [ "mode main" "layout h_tiles" "move-mouse window-lazy-center" ];
      backspace = [ "mode main" "close" ];
      comma = [ "mode main" "exec-and-forget ${rerunLastCommand}" ];
      # C-w itself, for the application.
      period = [ "mode main" "exec-and-forget ${pkgs.local.post-keystroke}/bin/post-keystroke ctrl w" ];
    };
in
{
  # Tiling, workspaces and the 'C-w' sigils mode, with the sigils drawn and
  # found by `sigils` (see tools/sigils).
  config = (if (builtins.hasAttr "aerospace" options.services)
  then {
    # mkIf rather than folding the check into the 'if' above: gating the shape
    # of 'config' on 'pkgs' recurses, since 'pkgs' comes from config.nixpkgs.
    services.aerospace = mkIf pkgs.stdenv.hostPlatform.isAarch64 {
      enable = true;

      settings = {
        default-root-container-layout = "tiles";
        default-root-container-orientation = "horizontal";

        # i3-style splits, so the sigils mode's '-' and '\' keys mean what they say
        # instead of being normalized away.
        enable-normalization-flatten-containers = false;
        enable-normalization-opposite-orientation-for-nested-containers = false;

        # Start with workspace 1 on the left monitor and 2 on the right.
        # (Force-assigning them would pin them, defeating 'summon-workspace'.)
        after-startup-command = [
          "focus-monitor left"
          "summon-workspace 1"
          "focus-monitor right"
          "summon-workspace 2"
          # Started here so that it has AeroSpace's Accessibility, for titles.
          # SketchyBar isn't a window, so its strip is kept from being an
          # empty space with a sigil.
          "exec-and-forget ${sigils} --daemon -s '${sigilPool}' -i top=40"
        ];

        on-window-detected = [
          # Tiled, then floated, the broot popup would be resized under broot.
          {
            "if".app-id = "net.kovidgoyal.kitty";
            "if".window-title-regex-substring = "^broot-popup$";
            run = "layout floating";
          }
          # AeroSpace restarting brings these windows back up.
          {
            "if".app-id = "com.electron.dockerdesktop";
            "if".during-aerospace-startup = true;
            run = "macos-native-minimize";
          }
          {
            "if".app-id = "com.cisco.secureclient.gui";
            "if".during-aerospace-startup = true;
            run = "macos-native-minimize";
          }
        ];

        # The pointer follows the focused window, unless it's already in it.
        on-focus-changed = [ "move-mouse window-lazy-center" ];

        # Moving it to a monitor with no windows would fight focusing the
        # monitor the pointer moves to.  (The module defaults this to
        # [ "move-mouse monitor-lazy-center" ].)
        on-focused-monitor-changed = [ ];

        # Keep under the 20 pixels `sigils` leaves out, or the gaps get
        # assigned sigils as if they were empty space.
        gaps = {
          inner.horizontal = 4;
          inner.vertical = 4;
          outer.left = 4;
          outer.right = 4;
          outer.top = 4;
          outer.bottom = 4;
        };

        mode.main.binding = {
          ctrl-w = "mode sigils";
          alt-shift-semicolon = "mode service";
        };

        mode.sigils.binding = sigilsMode;

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
