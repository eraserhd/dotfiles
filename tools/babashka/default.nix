{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.babashka ];
  };
}
