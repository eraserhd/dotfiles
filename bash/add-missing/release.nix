{ nixpkgs ? (import ./nixpkgs.nix), ... }:
let
  pkgs = import nixpkgs { config = {}; };
  add-missing = pkgs.callPackage ./derivation.nix {};

in rec {
  testEmptyDir = pkgs.runCommandNoCC "add-missing-test" {} ''
    mkdir -p $out
    cd $out
    ${add-missing}/bin/add-missing
    grep -q '^<nixpkgs>$' nixpkgs.nix
    grep -q '^/result' .gitignore
  '';
}
