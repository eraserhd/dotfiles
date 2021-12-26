{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.texlive.combined.scheme-tetex
    ];
  };
}
