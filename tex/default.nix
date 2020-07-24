{ pkgs, lib, ... }:

with lib;
{
  config = mkMerge [
    (mkIf pkgs.stdenv.isDarwin {
      homebrew.casks = [ "mactex" ];
    })
    (mkIf (!pkgs.stdenv.isDarwin) {
      environment.systemPackages = [
        pkgs.texlive.combined.scheme-tetex
      ];
    })
  ];
}
