{ nixpkgs ? (import ./nixpkgs.nix), ... }:
let
  pkgs = import nixpkgs { config = {}; };
  in-project = pkgs.callPackage ./derivation.nix {};
in {
  test = pkgs.runCommandNoCC "in-project-test" {} ''
    mkdir -p $out
    : ${in-project}
  '';
}