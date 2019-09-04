{ pkgs, ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".emacs".source = ./emacs;
    };
  };
}
