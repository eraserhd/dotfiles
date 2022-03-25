{ lib, config, options, pkgs, ... }:

with lib;
let
  cfg = config.local.updateDNS;
in {
  options = {
    local.updateDNS.enable = mkEnableOption "update DNS";
  };

  config = mkIf cfg.enable {
    local.scheduledJobs.updateDNS = {
      period = "5min";
      path = with pkgs; [ awscli curl gawk host ssmtp ];
      script = ''
        current_ip="$(curl -s http://ipinfo.io/ip)"
        dns_ip="$(host -t A ${config.networking.hostName}.${config.networking.domain} ns-112.awsdns-14.com |awk '$3 == "address"{print $4}')"
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

          hosted_zone_id=$(aws --profile=jason.m.felice route53 list-hosted-zones-by-name --dns-name="${config.networking.domain}." |sed -ne '
            /^ *"Id": /{
              s/^ *"Id": "//
              s,/hostedzone/,,
              s/",//
              p
            }
          ')

          AWS_PROFILE=jason.m.felice \
          aws route53 change-resource-record-sets \
              --hosted-zone-id "$hosted_zone_id" \
              --change-batch '{
              "Comment": "update-dns automatic update",
              "Changes": [ {
                  "Action": "UPSERT",
                  "ResourceRecordSet": {
                      "Name": "${config.networking.hostName}.${config.networking.domain}",
                      "Type": "A",
                      "TTL": 300,
                      "ResourceRecords": [ { "Value": "'"$current_ip"'" } ]
                  }
              } ]
          }'

          printf '\n'
        ) 2>&1 |sendmail jason.m.felice@gmail.com
      '';
    };
  };
}
