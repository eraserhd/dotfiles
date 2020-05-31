{ options, ... }:

{
  config = (if (builtins.hasAttr "wireless" options.networking)
  then {
    networking.wireless.networks = import ./networks.nix;
  }
  else {
  });
}
