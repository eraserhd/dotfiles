{ lib, config, options, pkgs, ... }:

with lib;
let
  cfg = config.local.updateDNS;

  updateDNSScript = pkgs.writeShellScriptBin "update-dns" ''
    current_ip="$(${pkgs.curl}/bin/curl -s http://ipinfo.io/ip)"
    dns_ip="$(${pkgs.host}/bin/host -t A crunch.eraserhead.net ns-112.awsdns-14.com |awk '$3 == "address"{print $4}')"
    if [[ $current_ip = $dns_ip ]]; then
        exit 0
    fi

    (
      printf 'Subject: crunch IP change\n'
      printf 'From: jfelice@crunch.eraserhead.net\n'
      printf '\n'
      printf 'Current IP: %s\n' "$current_ip"
      printf '    DNS IP: %s\n' "$dns_ip"
      printf '\n'

      AWS_PROFILE=jason.m.felice \
      ${pkgs.awscli}/bin/aws route53 change-resource-record-sets \
          --hosted-zone-id Z1035EO77EJRBA \
          --change-batch '{
          "Comment": "update-dns automatic update",
          "Changes": [ {
              "Action": "UPSERT",
              "ResourceRecordSet": {
                  "Name": "crunch.eraserhead.net",
                  "Type": "A",
                  "TTL": 300,
                  "ResourceRecords": [ { "Value": "'"$current_ip"'" } ]
              }
          } ]
      }'

      printf '\n'
    ) 2>&1 |sendmail jason.m.felice@gmail.com
  '';
in {
  options = {
    local.updateDNS.enable = mkEnableOption "update DNS";
  };

  config = mkIf cfg.enable (
  if (builtins.hasAttr "cron" options.services)
  then {
    services.cron = {
      enable = true;
      systemCronJobs = [
          "*/5 * * * *    jfelice  ${updateDNSScript}/bin/update-dns"
      ];
    };
  }
  else {
    assertions = [ {
      assertion = false;
      message = "local.updateDNS.enable is not supported on this system";
    } ];
  });
}
