{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.pom ];
  };
}
