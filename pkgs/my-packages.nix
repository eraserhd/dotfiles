{ stdenv, pkgs, ... }:

pkgs.buildEnv {
  name = "my-packages";
  paths = let
    linuxPackages = with pkgs; [
      texlive.combined.scheme-tetex
    ];
    osPackages = if stdenv.isDarwin
                 then []
                 else linuxPackages;
  in osPackages;
}
