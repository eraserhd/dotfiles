{ lib, ... }:

with lib;
{
  options = {
    local.scheduledJobs = mkOption {
      default = {};
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
}
