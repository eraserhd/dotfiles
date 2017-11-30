{ system ? builtins.currentSystem }:
let pkgs = import <nixpkgs> { inherit system; };
in
rec {
  _1password = import ./pkgs/1password {
    inherit (pkgs) stdenv fetchurl unzip;
  };
  anki = import ./pkgs/anki {
    inherit (pkgs) stdenv fetchurl undmg;
  };
  chez-scheme = import ./pkgs/chez-scheme {
    inherit (pkgs) stdenv fetchurl libiconv ncurses;
    inherit (pkgs.xorg) libX11;
  };
  slack = import ./pkgs/slack {
    inherit (pkgs) stdenv fetchurl unzip;
  };
}
