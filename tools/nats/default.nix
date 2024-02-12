{ config, lib, pkgs, ... }:

with lib;
let
  enable = config.local.kits.workstation.enable;
  isBrain = config.local.kits.brain.enable;

  token = builtins.readFile ./token.txt;

in {
  config = mkIf enable {
    environment.systemPackages = with pkgs; [
      natscli
    ];
    services.nats = {
      enable = true;
      jetstream = true;
      settings = {
        authorization = {
          inherit token;
        };
        jetstream = {
          max_memory_store = 128 * 1024 * 1024;
          max_file_store = 128 * 1024 * 1024;
        };
        leafnodes = {
          port = mkIf isBrain 7422;
          remotes = mkIf (!isBrain) [{
            url = "tls://${token}@crunch.eraserhead.net";
          }];
        };
      };
    };

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/nats/context.txt".text = "plugbench";
      home.file.".config/nats/context/plugbench.json".text = ''
        {
          "description": "Plugbench",
          "url": "nats://127.0.0.1:4222",
          "token": "${token}",
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
  };
}

