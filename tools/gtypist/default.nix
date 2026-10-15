{ config, pkgs, ... }:

{
  config = {
    home-manager.users.jfelice.home.packages = [ pkgs.gtypist ];
  };
}
