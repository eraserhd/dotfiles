{ ... }:

{
  homebrew.casks = [
    "hammerspoon"
  ];

  home-manager.users.jfelice = { pkgs, ... }: {
    home.file.".hammerspoon/init.lua".source = ./init.lua;
  };
}
