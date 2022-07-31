{ config, ... }:

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
  };
}
