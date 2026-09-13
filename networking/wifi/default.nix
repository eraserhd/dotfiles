{ config, lib, options, ... }:

with lib;
{
  config = (if (builtins.hasAttr "wireless" options.networking)
  then mkIf config.networking.wireless.enable {
    # PSKs live in the secrets file, referenced from networks.nix as
    # "ext:<name>"; see networking.wireless.secretsFile.
    age.secrets."wireless.conf".file = ./secrets.age;

    networking.wireless.secretsFile = config.age.secrets."wireless.conf".path;
    networking.wireless.networks = import ./networks.nix;
  }
  else {
  });
}
