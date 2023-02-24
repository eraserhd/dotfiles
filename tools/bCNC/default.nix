{ config, pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.bCNC ];
  };
}
