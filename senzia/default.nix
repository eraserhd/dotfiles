{ lib, config, ... }:

with lib;
let
  cfg = config.local.senzia;
in {
  options.local.senzia.enable = mkEnableOption "Senzia services";
  config = mkIf cfg.enable {
    services.couchdb = {
      enable = true;
    };
  };
}
