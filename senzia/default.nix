{ lib, config, pkgs, ... }:

with lib;
let
  cfg = config.local.senzia;
in {
  options.local.senzia.enable = mkEnableOption "Senzia services";
  config = mkIf cfg.enable {
    services.couchdb = {
      enable = true;
      bindAddress = "0.0.0.0";
      package = pkgs.couchdb2;
    };
    networking.firewall.allowedTCPPorts = [ 5984 6984 ];
  };
}
