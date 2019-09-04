{ pkgs, ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".gnupg/gpg.conf".source = ./gpg.conf;
    };
  };
}
