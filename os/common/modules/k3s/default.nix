{ lib, config, pkgs, ... }:

with lib;
let
  cfg = config.services.k3s;
in {
  options = {
    services.k3s.manifests = mkOption {
      type = types.listOf types.package;
      default = [];
      description = ''
        Manifest to apply on activation.
      '';
    };
  };
}
