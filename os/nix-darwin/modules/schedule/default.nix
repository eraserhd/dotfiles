{ lib, config, ... }:

let
  cfg = config.local.scheduledJobs;
in {
  config = lib.mkIf (cfg != {}) {
    assertions = [{
      assertion = false;
      message = "local.scheduledJobs not yet implemented on nix-darwin";
    }];
  };
}
