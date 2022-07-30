{ ... }:

{
  config = {
    services.nats = {
      enable = true;
      jetstream = true;
    };
  };
}
