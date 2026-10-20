{ pkgs, lib, options, config, ... }:

with lib;
let
  cfg = config.local.monitors;

  monitorsHere = pkgs.writeShellApplication {
    name = "monitors-here";
    runtimeInputs = [ pkgs.m1ddc ];
    text = ''
      status=0
      ${concatStrings (mapAttrsToList (uuid: input: ''
        m1ddc display ${uuid} set input ${toString input} || status=1
      '') cfg.inputs)}
      exit "$status"
    '';
  };

  # The monitors switch to the other machine when this one sleeps; this takes
  # them back on wake.  It runs while the screen is locked, but the monitors
  # may take a while to answer DDC after waking.
  monitorsHereOnWake = pkgs.writeShellApplication {
    name = "monitors-here-on-wake";
    runtimeInputs = [ monitorsHere pkgs.coreutils ];
    text = ''
      sleep 1
      for attempt in 1 2 3 4 5 6; do
        if error=$(monitors-here 2>&1); then
          exit 0
        fi
        [ "$attempt" -lt 6 ] && sleep 2
      done
      printf 'monitors-here failed: %s\n' "$error" >&2
      /usr/bin/osascript -e 'on run argv' \
        -e 'display notification (item 1 of argv) with title "monitors-here failed"' \
        -e 'end run' "$error"
      exit 1
    '';
  };
in {
  options.local.monitors.inputs = mkOption {
    type = types.attrsOf types.int;
    default = {};
    example = { "60B98E5F-79ED-400F-AA39-C3BFE065F094" = 17; };
    description = ''
      DDC input source of this machine on each monitor, by the UUID which
      `m1ddc display list` shows.  Read them with `m1ddc display <uuid> get
      input` while the monitor is showing this machine.  If set, installs
      `monitors-here`, which takes the monitors back after they've switched to
      another machine.
    '';
  };

  config = if (builtins.hasAttr "launchd" options)
  then mkIf (pkgs.stdenv.hostPlatform.isAarch64 && cfg.inputs != {}) {
    environment.systemPackages = [ monitorsHere ];

    # On waking, the system or just the displays, which -W both covers.
    home-manager.users.jfelice.launchd.agents.monitors-here-on-wake = {
      enable = true;
      config = {
        ProgramArguments = [
          "${pkgs.sleepwatcher}/bin/sleepwatcher"
          "-W" "${monitorsHereOnWake}/bin/monitors-here-on-wake"
        ];
        KeepAlive = true;
        RunAtLoad = true;
        StandardErrorPath = "/tmp/monitors-here-on-wake.log";
      };
    };
  }
  else {
    # Only declared here so hosts can set it.
  };
}
