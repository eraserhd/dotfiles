{ pkgs, config, ... }:

{
  config = {
    services.nats = {
      enable = true;
      serverName = config.local.systemDisplayName;

      jetstream = true;
      settings = {
        leafnodes = {
          remotes = [{
            url = "tls://connect.ngs.global";
            creds = ./ngs.creds;
          }];
        };
      };
    };

    local.browserCommand = "${pkgs.natscli}/bin/nats pub browser.open";
  };
}
