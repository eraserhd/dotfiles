{ lib, config, pkgs, ... }:

with lib;
let
  manifests = config.services.k3s.manifests;

  manifestDir = "/var/lib/rancher/k3s/server/manifests";

  installManifest = path: ''
    if [[ -d "${path}" ]]; then
      ${pkgs.rsync}/bin/rsync -q '${path}'/* /var/lib
    else
      name="${path}"
      name="''${name##*/}"
      name="''${name#*-}"
      cp "${path}" "${manifestDir}/$name"
    fi
  '';
in {
  options = {
    services.k3s.manifests = mkOption {
      type = types.listOf types.package;
      default = [];
      description = ''
        Manifest packages to set up in /var/lib/rancher/k3s/server/manifests on activation.
      '';
    };
  };

  config = {
    system.activationScripts.k3s-manifests = {
      text = concatMapStringsSep "\n" installManifest manifests;
    };
  };
}
