{ pkgs, ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".gnupg/gpg.conf".source = ./gpg.conf;
    };

    environment.systemPackages = with pkgs; [
      gnupg
      pinentry
    ];
  };
}
