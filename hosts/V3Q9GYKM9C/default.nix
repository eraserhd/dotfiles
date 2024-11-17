{ config, inputs, pkgs, ... }:

{
  nix.nixPath = [ {
    inherit (inputs) nixpkgs darwin;
  } ];

  programs.bash.enable = true;

  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
  };

  bubbles.enable = true;

  services.nix-daemon.enable = true;

  environment.xdgRuntimeDir.enable = true;

  users.users.jfelice = {
    name = "jfelice";        # Needed by home-manager
    home = "/Users/jfelice";
    shell = pkgs.zsh;
  };

  local.kits.workstation.enable = true;
  local.buildkite.enable = true;
  services.k3s.enable = true;

  plugbench.plumber.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  home-manager.users.jfelice.home.stateVersion = "22.05";

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.settings = {
    max-jobs = 2;
    cores = 4;
  };
}
