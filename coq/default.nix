{ config, lib, pkgs, ... }:

with lib;
{
  config = mkIf config.local.plan9.cpu.enable {
    environment.systemPackages = [ pkgs.coq ];
  };
}
