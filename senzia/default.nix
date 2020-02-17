{ lib, config, pkgs, ... }:

with lib;
let
  cfg = config.local.senzia;

  pemFile = pkgs.writeText "certificate.pem" ''
    ${builtins.readFile ./couchdb.crt}
    ${builtins.readFile ./RapidSSL_TLS_RSA_CA_G1.pem}
    ${builtins.readFile ./DigiCert_Global_Root_G2.pem}
    ${builtins.readFile ./couchdb.key}
  '';

in {
  options.local.senzia.enable = mkEnableOption "Senzia services";
  config = mkIf cfg.enable {
    services.couchdb = {
      enable = true;
      package = pkgs.couchdb2;
    };
    services.haproxy = {
      enable = true;
      config = ''
        frontend https-in
          bind *:6984 ssl crt ${pemFile}
          default_backend couchdb

        backend couchdb
          timeout connect 5000ms
          timeout client 50000ms
          timeout server 50000ms
          server couchdb1 127.0.0.1:5984 check inter 5s
      '';
    };
    networking.firewall.allowedTCPPorts = [ 6984 ];
    systemd.services.couchdb-backup = {
      script = ''
        set -ex
        export PATH="${pkgs.gnutar}/bin:${pkgs.gzip}/bin:$PATH"
        mkdir -p /var/lib/couchdb-backups/
        timestamp="$(date --iso-8601=seconds |sed -e 's/:/_/g')"
        cd /var/lib/couchdb/
        tar czf /var/lib/couchdb-backups/"$timestamp".tgz .
      '';
    };

    systemd.timers.couchdb-backup = {
      description = "Commit couchdb changes";
      partOf      = [ "couchdb-backup.service" ];
      wantedBy    = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "*:0/5";
        Persistent = true;
      };
    };
  };
}
