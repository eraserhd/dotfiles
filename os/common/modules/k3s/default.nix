{ lib, config, pkgs, ... }:

with lib;
let
  manifests = config.services.k3s.manifests;

  kubeconfig = "/etc/rancher/k3s/k3s.yaml";

  installManifest = path: ''
    ${pkgs.kubectl}/bin/kubectl --kubeconfig=${kubeconfig} apply -f ${path}
  '';
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

  config = {
    system.activationScripts.k3s-manifests = {
      text = ''
        if [[ -f ${kubeconfig} ]]; then
          printf '\e[36mInstalling k3s manifests...\e[0m\n'
          ${concatMapStringsSep "\n" installManifest manifests}
        fi
      '';
    };
  };
}
