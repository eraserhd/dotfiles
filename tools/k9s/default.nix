{ pkgs, ... }:

{
  config = {
    home-manager.users.jfelice.home.packages = [ pkgs.k9s ];
  };
}
