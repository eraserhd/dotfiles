{ lib, config, ... }:

with lib;
let
  cfg = config.local.scheduledJobs;
in {
  config = {
    systemd.timers = mapAttrs (name: job: {
      wantedBy = [ "timers.target" ];
      partOf = [ "${name}.service" ];
      timerConfig = {
        OnBootSec = job.period;
        OnUnitActiveSec = job.period;
        Unit = "${name}.service";
      };
    }) cfg;

    systemd.services = mapAttrs (name: job: {
      serviceConfig.Type = "oneshot";
      inherit (job) path script;
    }) cfg;
  };
}
