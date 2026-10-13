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

  workspace = pkgs.writeShellScript "sketchybar-workspace" ''
    sid=''${NAME#workspace.}
    focused=$(${aerospace} list-workspaces --focused)
    visible=$(${aerospace} list-workspaces --monitor all --visible)
    windows=$(${aerospace} list-windows --workspace "$sid" --count)

    if [ "$sid" = "$focused" ]; then
      sketchybar --set "$NAME" drawing=on label.color=0xffffffff background.drawing=on
    elif echo "$visible" | grep -qx "$sid"; then
      sketchybar --set "$NAME" drawing=on label.color=0xffffffff background.drawing=off
    elif [ "$windows" -gt 0 ]; then
      sketchybar --set "$NAME" drawing=on label.color=0xffc0c4dc background.drawing=off
    else
      sketchybar --set "$NAME" drawing=off
    fi
  '';

  tomat = pkgs.writeShellScript "sketchybar-tomat" ''
    sketchybar --set "$NAME" label="$(tomat status -o plain)"
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

        for sid in 1 2 3 4 5 6 7 8 9; do
          sketchybar --add item workspace.$sid left \
            --set workspace.$sid label=$sid script=${workspace} \
              click_script="${aerospace} workspace $sid" \
              background.color=0xff697098 background.corner_radius=5 \
              background.height=20 background.drawing=off \
              padding_left=2 padding_right=2 \
            --subscribe workspace.$sid aerospace_workspace_change front_app_switched
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

    services.aerospace.settings.exec-on-workspace-change = [
      "/bin/sh" "-c"
      "${config.services.sketchybar.package}/bin/sketchybar --trigger aerospace_workspace_change"
    ];

    services.aerospace.settings.gaps.outer.top = mkForce (barHeight + 2 * gap);
  }
  else {
  });
}
