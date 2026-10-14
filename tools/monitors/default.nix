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
  then {
    environment.systemPackages = mkIf (pkgs.stdenv.hostPlatform.isAarch64 && cfg.inputs != {}) [ monitorsHere ];
  }
  else {
    # Only declared here so hosts can set it.
  };
}
