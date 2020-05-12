{ ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".yabairc".source = ./yabairc;
    };
  };
}
