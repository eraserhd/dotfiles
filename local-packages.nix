{ system ? builtins.currentSystem }:
let pkgs = import <nixpkgs> { inherit system; };
in
rec {
  chez-scheme = import ./pkgs/chez-scheme {
    inherit (pkgs) stdenv fetchurl libiconv ncurses;
    inherit (pkgs.xorg) libX11;
  };
}
