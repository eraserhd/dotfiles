{ lib, pkgs, ... }:

with lib;
{
  config = mkIf (!pkgs.stdenv.isDarwin) {
    environment.systemPackages = with pkgs; [
      pciutils
    ];
  };
}
