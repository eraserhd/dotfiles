{ config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.k3s;
in {
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ k3s kubectl ];
    services.k3s = {
      role = "server";
    };
    system.activationScripts.k3s-manifests = {
      text = ''
        printf '\e[36mInstalling k3s manifests...\e[0m\n'
        ${concatMapStringsSep "\n" (manifest: ''
          ${pkgs.kubectl}/bin/kubectl --kubeconfig="/etc/rancher/k3s/k3s.yaml" apply -f "${manifest}"
        '') cfg.manifests}
      '';
    };
  };
}
