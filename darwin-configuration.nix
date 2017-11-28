{ config, lib, pkgs, ... }:
{

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
      awscli
      bash-completion
      ghc
      git
      git-crypt
      gnupg
      go
      graphviz
      idris
      leiningen
      nix-repl
      nodejs
      python27Full
      reattach-to-user-namespace
      rlwrap
      sassc
      stack
      terraform
      tmate
      tmux
      vault
      wget
    ];

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;

  # Recreate /run/current-system symlink after boot.
  services.activate-system.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 2;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 2;
}
