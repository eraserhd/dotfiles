{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.nats;

  format = pkgs.formats.json { };

  configFile = format.generate "nats.conf" cfg.settings;

in {
  options = {
    services.nats = {
      enable = mkEnableOption (mdDoc "NATS messaging system");

      user = mkOption {
        type = types.str;
        default = "nats";
        description = mdDoc "User account under which NATS runs.";
      };

      group = mkOption {
        type = types.str;
        default = "nats";
        description = mdDoc "Group under which NATS runs.";
      };

      serverName = mkOption {
        default = "nats";
        example = "n1-c3";
        type = types.str;
        description = mdDoc "Name of the NATS server, must be unique if clustered.";
      };

      jetstream = mkEnableOption (mdDoc "JetStream");

      port = mkOption {
        default = 4222;
        type = types.port;
        description = mdDoc "Port on which to listen";
      };

      dataDir = mkOption {
        default = "/private/var/lib/nats";
        type = types.path;
        description = mdDoc ''
          The NATS data directory. Only used if JetStream is enabled, for
          storing stream metadata and messages.

          If left as the default value this directory will automatically be
          created before the NATS server starts, otherwise the sysadmin is
          responsible for ensuring the directory exists with appropriate
          ownership and permissions.
        '';
      };

      settings = mkOption {
        default = { };
        type = format.type;
        example = literalExpression ''
          {
            jetstream = {
              max_mem = "1G";
              max_file = "10G";
            };
          };
        '';
        description = mdDoc ''
          Declarative NATS configuration. See the
          [
          NATS documentation](https://docs.nats.io/nats-server/configuration) for a list of options.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.nats.settings = {
      server_name = cfg.serverName;
      port = cfg.port;
      jetstream = optionalAttrs cfg.jetstream { store_dir = cfg.dataDir; };
    };

    launchd.daemons.nats = {
      path = with pkgs; [ nats-server ];
      script = ''
        nats-server -c ${configFile}
      '';
      serviceConfig = {
        KeepAlive = true;
        UserName = cfg.user;
        GroupName = cfg.group;
        StandardOutPath = "/var/log/nats.out.log";
        StandardErrorPath = "/var/log/nats.err.log";
      };
    };

    system.activationScripts.preActivation.text = ''
      mkdir -p '${cfg.dataDir}'
      touch /var/log/nats.out.log /var/log/nats.err.log
      chown 4222:4222 '${cfg.dataDir}' /var/log/nats.out.log /var/log/nats.err.log
    '';

    users.knownUsers = ["nats"];
    users.knownGroups = ["nats"];

    users.users = mkIf (cfg.user == "nats") {
      nats = {
        uid = 4222;
        description = "NATS daemon user";
        home = cfg.dataDir;
      };
    };

    users.groups = mkIf (cfg.group == "nats") {
      nats = {
        gid = 4222;
        description = "NATS daemon group";
        members = [ cfg.user ];
      };
    };
  };
}
