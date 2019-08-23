{ nixpkgs ? (import ./nixpkgs.nix), ... }:
let
  pkgs = import nixpkgs { config = {}; };
  add-missing = pkgs.callPackage ./derivation.nix {};

in rec {
  test = pkgs.runCommandNoCC "add-missing-test" {} ''
    mkdir -p $out
    cd $out
    ${add-missing}/bin/add-missing
    [[ -f nixpkgs.nix ]]
  '';
}
