{ ... }:

{
  config = {
    services.nats = {
      enable = true;
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
