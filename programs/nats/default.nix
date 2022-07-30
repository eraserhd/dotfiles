{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.natscli ];
  };
}
