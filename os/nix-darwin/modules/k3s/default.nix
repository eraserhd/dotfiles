{ lib, config, pkgs, ... }:

with lib;
let
  cfg = config.services.k3s;

  # This redirects pacakges to use the rancher-installed versions.  An overlay
  # is used so that other modules can still refer to these packages to do things
  # like run kubectl.
  useRancherToolsOverlay = self: super: let
    rancher-wrapper = super.callPackage ./rancher-wrapper {};
  in {
    docker-compose = rancher-wrapper;
    docker-buildx = rancher-wrapper;
    helm = rancher-wrapper;
    kubectl = rancher-wrapper;
    nerdctl = rancher-wrapper;

    inherit rancher-wrapper;
  };

in {
  options = {
    services.k3s.enable = mkEnableOption "k3s via Rancher Desktop";
  };

  config = mkIf cfg.enable {
    homebrew.casks = [ "rancher" ];
    environment.systemPackages = with pkgs; [ rancher-wrapper ];
    nixpkgs.overlays = [ useRancherToolsOverlay ];

    services.k3s.localKubectlWrapper = pkgs.writeShellScriptBin "kubectl" ''
      kubeconfig="$(mktemp)"
      trap "rm -f $kubeconfig" EXIT
      ${pkgs.rancher-wrapper}/bin/rdctl shell sudo cat /etc/rancher/k3s/k3s.yaml >"$kubeconfig"
      ${pkgs.rancher-wrapper}/bin/kubectl --kubeconfig="$kubeconfig" "$@"
    '';
  };
}
