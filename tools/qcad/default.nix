{ lib, pkgs, ... }:

with lib; {
  config = {
    homebrew.casks = mkIf pkgs.stdenv.isDarwin [ "qcad" ];
    environment.systemPackages = mkIf (!pkgs.stdenv.isDarwin) [ pkgs.qcad ];
  };
}
