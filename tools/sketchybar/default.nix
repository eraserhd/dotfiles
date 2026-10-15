{ pkgs, lib, config, options, ... }:

with lib;
let
  barHeight = 32;
  gap = 4; # Same as the AeroSpace gaps.

  frontApp = pkgs.writeShellScript "sketchybar-front-app" ''
    if [ "$SENDER" = front_app_switched ]; then
      sketchybar --set "$NAME" label="$INFO"
    fi
  '';

  aerospace = "${config.services.aerospace.package}/bin/aerospace";

  # Each monitor's bar shows the workspace on that monitor, highlighted when
  # the monitor is focused.  The item is named for the monitor's position from
  # the left, which is how AeroSpace numbers monitors.
  monitorWorkspace = pkgs.writeShellScript "sketchybar-monitor-workspace" ''
    monitor=''${NAME#monitor.}
    workspace=$(${aerospace} list-workspaces --monitor "$monitor" --visible)
    focused=$(${aerospace} list-monitors --focused --format '%{monitor-id}')

    if [ "$monitor" = "$focused" ]; then
      sketchybar --set "$NAME" label="$workspace" label.color=0xffffffff background.drawing=on
    else
      sketchybar --set "$NAME" label="$workspace" label.color=0xffc0c4dc background.drawing=off
    fi
  '';

  tomat = pkgs.writeShellScript "sketchybar-tomat" ''
    sketchybar --set "$NAME" label="$(${pkgs.tomat}/bin/tomat status -o plain)"
  '';

  battery = pkgs.writeShellScript "sketchybar-battery" ''
    percent=$(pmset -g batt | grep -Eo '[0-9]+%')
    if pmset -g batt | grep -q 'AC Power'; then
      percent="⚡$percent"
    fi
    sketchybar --set "$NAME" label="$percent"
  '';

  clock = pkgs.writeShellScript "sketchybar-clock" ''
    sketchybar --set "$NAME" label="$(date '+%a %d %b %H:%M')"
  '';
in {
  config = (if (builtins.hasAttr "sketchybar" options.services)
  then {
    services.sketchybar = {
      enable = true;
      config = ''
        #!/bin/sh

        sketchybar --bar height=${toString barHeight} position=top color=0xff292d3e \
          margin=${toString gap} y_offset=${toString gap} corner_radius=9 \
          border_width=2 border_color=0xff65697b

        sketchybar --default \
          label.font="mononoki:Regular:13.0" \
          label.color=0xffc0c4dc \
          padding_left=6 padding_right=6

        sketchybar --add event aerospace_workspace_change

        # A bar is drawn on each display, and the display ids are in the order
        # macOS arranged them, so map them to positions.
        sketchybar --query displays \
          | ${pkgs.jq}/bin/jq -r 'sort_by(.frame.x) | to_entries[] | "\(.key + 1) \(.value["arrangement-id"])"' \
          | while read -r monitor display; do
          sketchybar --add item monitor.$monitor left \
            --set monitor.$monitor associated_display=$display \
              script=${monitorWorkspace} \
              background.color=0xff697098 background.corner_radius=5 \
              background.height=20 background.drawing=off \
              padding_left=2 padding_right=2 \
            --subscribe monitor.$monitor aerospace_workspace_change front_app_switched
        done

        sketchybar --add item front_app left \
          --set front_app script=${frontApp} \
          --subscribe front_app front_app_switched

        sketchybar --add item clock right \
          --set clock update_freq=30 script=${clock}

        sketchybar --add item battery right \
          --set battery update_freq=120 script=${battery} \
          --subscribe battery power_source_change system_woke

        sketchybar --add item tomat right \
          --set tomat update_freq=1 script=${tomat}

        sketchybar --update
      '';
    };

    launchd.user.agents.sketchybar.serviceConfig.EnvironmentVariables =
      optionalAttrs config.environment.xdgRuntimeDir.enable {
        XDG_RUNTIME_DIR = "${config.users.users.${config.system.primaryUser}.home}/.run";
      };

    services.aerospace.settings = let
      trigger = [
        "/bin/sh" "-c"
        "${config.services.sketchybar.package}/bin/sketchybar --trigger aerospace_workspace_change"
      ];
    in {
      exec-on-workspace-change = trigger;
      on-focused-monitor-changed = trigger;
      gaps.outer.top = mkForce (barHeight + 2 * gap);
    };
  }
  else {
  });
}
