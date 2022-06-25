{ config, inputs, pkgs, ... }:

{
  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/src/dotfiles/machines/macbook/default.nix
  environment.darwinConfig = "$HOME/src/dotfiles/machines/macbook/default.nix";

  nix.nixPath = [ {
    inherit (inputs) nixpkgs darwin;
    darwin-config = "$HOME/src/dotfiles/machines/macbook/default.nix";
  } ];

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  local.systemDisplayName = "macbook";

  programs.bash.enable = true;

  homebrew.enable = true;
  homebrew.cleanup = "zap";
  homebrew.extraConfig = ''
    cask_args appdir: "/Applications"
  '';

  bubbles.enable = true;

  services.nix-daemon.enable = true;

  environment.xdgRuntimeDir.enable = true;

  environment.systemPackages = with pkgs; [
    qemu
  ];

  users.users.jfelice = {
    name = "jfelice";        # Needed by home-manager
    home = "/Users/jfelice";
    shell = pkgs.zsh;
  };

  local.plan9.terminal.enable = true;
  local.buildkite.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  home-manager.users.jfelice.home.stateVersion = "22.05";

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 2;
  nix.buildCores = 4;
}
