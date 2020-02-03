{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.gambit ];
  };
}
