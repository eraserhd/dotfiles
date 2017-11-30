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
  mactex = import ./pkgs/mactex {
    inherit (pkgs) stdenv fetchurl xar cpio;
  };
  slack = import ./pkgs/slack {
    inherit (pkgs) stdenv fetchurl unzip;
  };
}
