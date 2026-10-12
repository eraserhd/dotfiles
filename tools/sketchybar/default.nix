{ pkgs, lib, config, options, ... }:

with lib;
let
  barHeight = 28;
  gap = 4; # Same as the AeroSpace gaps.

  frontApp = pkgs.writeShellScript "sketchybar-front-app" ''
    if [ "$SENDER" = front_app_switched ]; then
      sketchybar --set "$NAME" label="$INFO"
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

    # The menu bar is hidden (see bubbles), so nothing else reserves this space.
    # Bar, then a gap above and below it.
    services.aerospace.settings.gaps.outer.top = mkForce (barHeight + 2 * gap);
  }
  else {
  });
}
