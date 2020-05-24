{ ... }:

{
  config = {
    networking.wireless.networks = import ./networks.nix;
  };
}
