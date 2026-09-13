{ config, lib, options, pkgs, ... }:

with lib;
let
  isDarwin = builtins.hasAttr "launchd" options;

  tokenFile = config.local.nats.clientTokenFile;
in {
  options = {
    local.kits.workstation.enable = mkEnableOption "workstation";
  };

  config = mkMerge [
    {
      # plugbench.token is deliberately left null: it would bake the token
      # into the store.  Every pluggo reads $NATS_TOKEN, so it is supplied
      # from the agenix file at run time instead.
      plugbench.clipboard.enable = true;
      plugbench.kakoune.enable = true;
      plugbench.plumber.client = true;
    }

    (if isDarwin
     then {
       launchd.user.agents = mkMerge [
         (mkIf config.plugbench.clipboard.enable {
           plugbench-clipboard.script = mkForce ''
             set -a
             . ${tokenFile}
             set +a
             exec ${pkgs.clipboard-pluggo}/bin/clipboard
           '';
         })
         (mkIf config.plugbench.plumber.enable {
           plugbench-plumber.script = mkForce ''
             set -a
             . ${tokenFile}
             set +a
             exec ${pkgs.plumber-pluggo}/bin/plumber
           '';
         })
       ];
     }
     else mkMerge [
       (mkIf config.plugbench.plumber.enable {
         # "-": other users can't read jfelice's token, and shouldn't.
         systemd.user.services.plugbench-plumber.serviceConfig.EnvironmentFile =
           "-${tokenFile}";
       })
       (mkIf config.plugbench.clipboard.enable {
         services.xserver.displayManager.sessionCommands = mkBefore ''
           if [ -r ${tokenFile} ]; then
             set -a
             . ${tokenFile}
             set +a
           fi
         '';
       })
     ])
  ];
}
