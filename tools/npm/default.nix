{ pkgs, ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".npmrc".source = ./npmrc;
    };
  };
}
