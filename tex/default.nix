{ pkgs, lib, ... }:

{
  config = lib.mkIf (!pkgs.stdenv.isDarwin) {
    environment.systemPackages = [
      pkgs.texlive.combined.scheme-tetex
    ];
  };
}
