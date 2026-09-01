{ config, inputs, pkgs, ... }:

{
  nix.nixPath = [ {
    inherit (inputs) nixpkgs darwin;
  } ];

  programs.bash.enable = true;
  plugbench.plumber.enable = true;

  local.no1password = true;
  local.kits.workstation.enable = true;
  #services.k3s.enable = true;

  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
    prefix = "/opt/workbrew";
  };

  bubbles.enable = true;
  environment.xdgRuntimeDir.enable = true;

  ids.gids.nixbld = 350;
  system.primaryUser = "jfelice";
  users.users.jfelice = {
    name = "jfelice";        # Needed by home-manager
    home = "/Users/jfelice";
    shell = pkgs.zsh;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  home-manager.users.jfelice.home.stateVersion = "22.05";

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.settings = {
    max-jobs = 2;
    cores = 6;
  };

  # Market Risk team requirements
  services.postgresql = {
    enable = true;
    authentication = ''
       local   all all trust
       hostssl all all 127.0.0.1/32 md5
    '';
  };

  homebrew.casks = [
    "kreya"
  ];
}
