{ nixpkgs ? (import ./nixpkgs.nix), ... }:
let
  pkgs = import nixpkgs { config = {}; };
  add-missing = pkgs.callPackage ./derivation.nix {};

in rec {
  test = pkgs.runCommandNoCC "add-missing-test" {} ''
    set -x

    testCase() {
      mkdir -p "$out/$1"
      cd "$out/$1"
      return $?
    }

    testCase empty-dir
    ${add-missing}/bin/add-missing
    grep -q '^<nixpkgs>$' nixpkgs.nix
    grep -q '^/result' .gitignore
    grep -q '^= empty-dir$' README.adoc
    grep -q '^use nix$' .envrc
    [[ $(cat overlay.nix) = 'self: super: {
  empty-dir = super.callPackage ./derivation.nix {};
}' ]]
    [[ $(cat default.nix) = 'let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs {
    config = {};
    overlays = [ (import ./overlay.nix) ];
  };

in pkgs.empty-dir' ]]

    testCase gitignore-no-result
    printf '/foo\n' >.gitignore
    ${add-missing}/bin/add-missing
    grep -q '^/foo$' .gitignore
    grep -q '^/result$' .gitignore
    grep -q 'http://unlicense.org/' UNLICENSE

    testCase gitignore-yes-result
    printf '/result\n' >.gitgnore
    ${add-missing}/bin/add-missing
    (( 1 == $(grep -c '^/result$' .gitignore) ))

    testCase has-README-md
    touch README.md
    ${add-missing}/bin/add-missing
    [[ ! -f README.adoc ]]

    testCase bare-REDME
    touch README
    ${add-missing}/bin/add-missing
    [[ ! -f README.adoc ]]

    testCase has-envrc
    touch .envrc
    ${add-missing}/bin/add-missing
    (( 0 == $(grep -c '^use nix$' .envrc) ))

    testCase has-overlay-nix
    printf 'xxx\n' >overlay.nix
    ${add-missing}/bin/add-missing
    [[ xxx = $(cat overlay.nix) ]]

    testCase has-default-nix
    printf 'xyz\n' >default.nix
    ${add-missing}/bin/add-missing
    [[ xyz = $(cat default.nix) ]]

    testCase has-LICENSE
    printf 'xyz\n' >LICENSE.md
    ${add-missing}/bin/add-missing
    [[ ! -f UNLICENSE ]]

    set +x
  '';
}
