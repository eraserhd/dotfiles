{ config, lib, pkgs, ... }:
let local = import ./local-packages.nix { };
in
{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = (with pkgs; [
      awscli
      chez
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
    ]) ++ (with local; [
      _1password
      slack
    ]);

  nixpkgs.config.allowUnfree = true;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;
  programs.bash.enableCompletion = true;

  # Recreate /run/current-system symlink after boot.
  services.activate-system.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 2;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 2;
}
