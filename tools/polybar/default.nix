{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.polybar ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/polybar/config.ini".source = ./config.ini;
    };
  };
}
