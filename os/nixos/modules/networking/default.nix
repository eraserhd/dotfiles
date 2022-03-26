{ config, ... }:

{
  config = {
    networking.nameservers = config.local.networking.nameservers;
  };
}
