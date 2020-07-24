{ config, pkgs, ... }:

{
  imports = [
    ../../2u
    ../../common.nix
    ../../home-manager/nix-darwin
    ../../modules/nix-darwin
  ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/src/dotfiles/machines/macbook/default.nix
  environment.darwinConfig = "$HOME/src/dotfiles/machines/macbook/default.nix";

  nix.nixPath = [ {
    nixpkgs = "$HOME/src/dotfiles/nixpkgs";
    darwin = "$HOME/src/dotfiles/nix-darwin";
    darwin-config = "$HOME/src/dotfiles/machines/macbook/default.nix";
  } ];

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  local.systemDisplayName = "macbook";

  programs.bash.enable = true;

  homebrew.enable = true;
  homebrew.cask_args.appdir = "/Applications";

  # System settings
  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.NSGlobalDomain.AppleKeyboardUIMode = 3;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 20;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;
  system.defaults.NSGlobalDomain."com.apple.keyboard.fnState" = true;

  bubbles.enable = true;

  services.nix-daemon.enable = true;

  services.autossh.sessions = [ {
    name = "crunch";
    user = "jfelice";
    extraArguments = " -i ${toString ../../networking/ssh/files/id_rsa-macbook}" +
                     " -o 'StreamLocalBindUnlink yes'" +
                     " -o 'ExitOnForwardFailure yes'" +
                     " -o 'ServerAliveCountMax 3'" +
                     " -o 'ServerAliveInterval 10'" +
                     " -L8820:localhost:8820" +
                     " -L8080:localhost:8080" +
                     " -L3449:localhost:3449" +
                     " -R14242:localhost:14242" + # CP dev repl
                     " -R24242:localhost:24242" + # CP prod repl
                     " -R/run/user/1000/plan9/srv/snarf:/Users/jfelice/.run/plan9/srv/snarf" +
                     " -R/run/user/1000/plan9/srv/plumb:/Users/jfelice/.run/plan9/srv/plumb" +
                     " -T -N jfelice@crunch.eraserhead.net";
  } ];

  environment.xdgRuntimeDir.enable = true;

  users.users.jfelice = {
    name = "jfelice";        # Needed by home-manager
    home = "/Users/jfelice";
    shell = pkgs.zsh;
  };

  services."2u".vault.enable = true;
  services."2u".kubernetes-clients.enable = true;
  services."2u".kubernetes-clients.namespaces = [ "implementation" ];

  local.plan9.terminal.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 1;
  nix.buildCores = 4;
}
