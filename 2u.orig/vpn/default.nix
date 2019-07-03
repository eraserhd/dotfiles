{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.services."2u".vpn;
in {
  options.services."2u".vpn = {
    enable = mkEnableOption "2U VPN";
  };

  config = mkIf cfg.enable {
    services.strongswan.enable = true;
    services.strongswan.connections = {
      "2u" = {
        authby = "secret";
        auto = "start";
        keyexchange = "ikev1";
        type = "transport";
        left = "%any";
        right = "client.vpn.2tor.net";
      };
    };
    services.strongswan.secrets = [ "${./ipsec.secrets}" ];
  };
}
