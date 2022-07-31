{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.services.nats;

  format = pkgs.formats.json { };

  configFile = format.generate "nats.conf" cfg.settings;

in {

  ### Interface

  options = {
    services.nats = {
      enable = mkEnableOption "NATS messaging system";

      user = mkOption {
        type = types.str;
        default = "nats";
        description = "User account under which NATS runs.";
      };

      group = mkOption {
        type = types.str;
        default = "nats";
        description = "Group under which NATS runs.";
      };

      serverName = mkOption {
        default = "nats";
        example = "n1-c3";
        type = types.str;
        description = ''
          Name of the NATS server, must be unique if clustered.
        '';
      };

      jetstream = mkEnableOption "JetStream";

      port = mkOption {
        default = 4222;
        type = types.port;
        description = ''
          Port on which to listen.
        '';
      };

      dataDir = mkOption {
        default = "/var/lib/nats";
        type = types.path;
        description = ''
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
        description = ''
          Declarative NATS configuration. See the
          <link xlink:href="https://docs.nats.io/nats-server/configuration">
          NATS documentation</link> for a list of options.
        '';
      };
    };
  };

  ### Implementation

  config = mkIf cfg.enable {
    services.nats.settings = {
      server_name = cfg.serverName;
      port = cfg.port;
      jetstream = optionalAttrs cfg.jetstream { store_dir = cfg.dataDir; };
    };

    launchd.daemons.nats = {
      command = "${pkgs.nats-server}/bin/nats-server -c ${configFile}";
      serviceConfig = {
        KeepAlive = true;
        UserName = cfg.user;
        GroupName = cfg.group;
      };
    };

    users.users = mkIf (cfg.user == "nats") {
      nats = {
        uid = 4222;
        description = "NATS daemon user";
        createHome = true;
        home = cfg.dataDir;
      } // (if cfg.group == "nats"
            then { gid = 4222; }
            else {});
    };

    users.knownUsers = [ "nats" ];
    users.knownGroups = [ "nats" ];

    users.groups = mkIf (cfg.group == "nats") {
      nats = {
        gid = 4222;
        description = "NATS daemon group";
        members = [ cfg.user ];
      };
    };
  };

}
