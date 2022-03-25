{ lib, config, ... }:

with lib;
let
  cfg = config.local.scheduledJobs;
in {
  options = {
    local.scheduledJobs = mkOption {
      description = ''
        Lowest-common-denominator, OS-independent scheduled jobs.
      '';
      type = types.attrsOf (types.submodule {
        options = {
          period = mkOption {
            description = ''
              How often the job runs.
            '';
            example = "5min";
            type = types.str;
          };
          path = mkOption {
            default = [];
            type = with types; listOf (oneOf [ package str ]);
            description = ''
              Packages to add to the job's <envar>PATH</envar>
              environment variable.  Both the <filename>bin</filename>
              and <filename>sbin</filename> subdirectories of each
              package are added.
            '';
          };
          script = mkOption {
            description = ''
              Bash shell code to run.
            '';
            example = ''
              echo hello >>/tmp/log
            '';
            type = types.lines;
          };
        };
      });
    };
  };

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
