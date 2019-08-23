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

    testCase gitignore-no-result
    printf '/foo\n' >.gitignore
    ${add-missing}/bin/add-missing
    grep -q '^/foo$' .gitignore
    grep -q '^/result$' .gitignore

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

    set +x
  '';
}
