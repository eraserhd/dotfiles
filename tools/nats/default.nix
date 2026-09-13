{ config, lib, options, pkgs, ... }:

with lib;
let
  isDarwin = builtins.hasAttr "launchd" options;

  format = pkgs.formats.json { };

  # The same settings the nats module would render itself, except that
  # authorization.token is still the literal "$NATS_TOKEN".  nats-server does
  # not expand variables inside quoted strings (and JSON has no `include`), so
  # the real config is rendered at start-up instead, into a directory only the
  # nats user can read.
  configTemplate = format.generate "nats.conf.in" config.services.nats.settings;

  runtimeConfig = "/run/nats/nats.conf";

  # Usage: render-nats-config <env-file>
  renderConfig = pkgs.writeShellScript "render-nats-config" ''
    set -eu
    set -a
    . "$1"
    set +a
    umask 0077
    ${pkgs.envsubst}/bin/envsubst < ${configTemplate} > ${runtimeConfig}
  '';

  serverToken = config.age.secrets."nats-token.env".path;
in {
  options = {
    local.nats.clientTokenFile = mkOption {
      type = types.str;
      description = ''
        File containing NATS_TOKEN=..., readable by jfelice.  Anything that
        connects to the local NATS server (natscli, the pluggos) should source
        it or take it as an EnvironmentFile rather than having the token baked
        into the store.
      '';
    };
  };

  config = mkMerge [
    {
      environment.systemPackages = with pkgs; [
        natscli
      ];

      # The token, as NATS_TOKEN=..., installed twice from the same file: once
      # for the server and once for everything that connects to it.
      age.secrets."nats-token.env" = {
        file = ./token.env.age;
        owner = config.services.nats.user;
      };
      age.secrets."nats-token-client.env" = {
        file = ./token.env.age;
        owner = "jfelice";
      };

      local.nats.clientTokenFile = config.age.secrets."nats-token-client.env".path;

      services.nats = {
        enable = true;
        jetstream = true;
        settings = {
          authorization = {
            token = "$NATS_TOKEN";
          };
          jetstream = {
            max_memory_store = 128 * 1024 * 1024;
            max_file_store = 128 * 1024 * 1024;
          };
          leafnodes = {
            port = 7422;
          };
        };
      };

      home-manager.users.jfelice = { pkgs, ... }: {
        home.file.".config/nats/context.txt".text = "plugbench";
        # No token here: natscli reads $NATS_TOKEN.
        home.file.".config/nats/context/plugbench.json".text = ''
          {
            "description": "Plugbench",
            "url": "nats://127.0.0.1:4222",
            "token": "",
            "user": "",
            "password": "",
            "creds": "",
            "nkey": "",
            "cert": "",
            "key": "",
            "ca": "",
            "nsc": "",
            "jetstream_domain": "",
            "jetstream_api_prefix": "",
            "jetstream_event_prefix": "",
            "inbox_prefix": "",
            "user_jwt": ""
          }
        '';
      };
    }

    (if isDarwin
     then {
       # Rendering happens in the launchd script; see os/nix-darwin/modules/nats.
       services.nats.environmentFile = serverToken;
     }
     else {
       systemd.services.nats.serviceConfig = {
         EnvironmentFile = serverToken;
         RuntimeDirectory = "nats";
         RuntimeDirectoryMode = "0700";
         ExecStartPre = "${renderConfig} ${serverToken}";
         ExecStart = mkForce "${pkgs.nats-server}/bin/nats-server -c ${runtimeConfig}";
       };
     })
  ];
}
