{ pkgs, ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".gnupg/gpg-agent.conf".text = ''
        pinentry-program ${pkgs.pinentry-curses}/bin/pinentry-curses
      '';
      home.file.".gnupg/gpg.conf".source = ./gpg.conf;
    };

    environment.systemPackages = with pkgs; [
      gnupg
      pinentry-curses
    ];
  };
}
