{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.nexus-tools ];
  };
}
