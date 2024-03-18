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

    programs.zsh.interactiveShellInit = ''
      autoload -Uz compinit
      compinit
      source <(${pkgs.rancher-wrapper}/bin/kubectl completion zsh)
    '';
    programs.bash.interactiveShellInit = ''
      source <(${pkgs.rancher-wrapper}/bin/kubectl completion bash)
    '';
    programs.fish.interactiveShellInit = ''
      ${pkgs.rancher-wrapper}/bin/kubectl completion fish |source
    '';
  };
}
