{ lib, config, pkgs, ... }:

with lib;
let
  cfg = config.services.k3s;

  defaultKubectlWrapper = pkgs.writeShellScriptBin "kubectl" ''
    exec ${pkgs.kubectl}/bin/kubectl --kubeconfig="/etc/rancher/k3s/k3s.yaml" "$@"
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

    services.k3s.localKubectlWrapper = mkOption {
      type = types.package;
      default = defaultKubectlWrapper;
      description = ''
        Wrapper for kubectl used to apply manifests on activation.
      '';
    };
  };

  config = {
    system.activationScripts.postUserActivation = {
      text = ''
        printf '\e[36mInstalling k3s manifests...\e[0m\n'
        ${concatMapStringsSep "\n" (manifest: "${cfg.localKubectlWrapper}/bin/kubectl apply -f \"${manifest}\"") cfg.manifests}
      '';
    };
  };
}
