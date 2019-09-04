{ pkgs, ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".vimrc".source = ./vimrc;
    };
  };
}
