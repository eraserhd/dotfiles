{ lib, config, pkgs, ... }:

with lib;
let
  cfg = config.local.updateDNS;
  updateDNSScript = pkgs.writeShellScriptBin "update-dns" ''
    #!${pkgs.bash}/bin/bash

    ${builtins.readFile ../bin/private.sh}

    exec ${pkgs.curl}/bin/curl -s -X PUT -H "Content-Type: application/json" \
      -H "Authorization: Bearer $DIGITALOCEAN_API_TOKEN" \
      -d '{"data": "'"$(curl -s http://ipinfo.io/ip)"'"}' \
      "https://api.digitalocean.com/v2/domains/eraserhead.net/records/73014284" \
      >/dev/null
  '';
in {
  options = {
    local.updateDNS.enable = mkEnableOption "update DNS";
  };

  config = mkIf cfg.enable {
    services.cron = {
      enable = true;
      systemCronJobs = [
          "*/5 * * * *    jfelice  ${updateDNSScript}/bin/update-dns"
      ];
    };
  };
}
