{ lib, pkgs, ... }:

with lib;
{
  config = mkMerge [
    {
      home-manager.users.jfelice = { pkgs, ... }: {
        home.file.".gnupg/gpg.conf".source = ./gpg.conf;
      };

      environment.systemPackages = with pkgs; [
        gnupg
      ];
    }
    # pinentry-curses somehow requires gcr which isn't available on Mac
    (mkIf (!pkgs.stdenv.isDarwin) {
      home-manager.users.jfelice = { pkgs, ... }: {
        home.file.".gnupg/gpg-agent.conf".text = ''
          pinentry-program ${pkgs.pinentry-curses}/bin/pinentry-curses
        '';
      };
      environment.systemPackages = with pkgs; [
        pinentry-curses
      ];
    })
  ];
}
