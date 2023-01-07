{ config, lib, pkgs, ... }:

with lib;
let
  enable = config.local.kits.workstation.enable;
in {
  config = mkIf enable {
    environment.systemPackages = with pkgs; [
      natscli
    ];
    services.nats = {
      enable = true;
      jetstream = true;
      settings = {
        jetstream = {
          max_memory_store = 128 * 1024 * 1024;
          max_file_store = 128 * 1024 * 1024;
        };
        leafnodes = {
          remotes = [{
            url = "tls://connect.ngs.global";
            credentials = ./ngs.creds;
          }];
        };
      };
    };
  };
}

