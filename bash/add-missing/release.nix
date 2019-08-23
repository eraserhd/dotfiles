{ nixpkgs ? (import ./nixpkgs.nix), ... }:
let
  pkgs = import nixpkgs { config = {}; };
  add-missing = pkgs.callPackage ./derivation.nix {};

in rec {
  test = pkgs.runCommandNoCC "add-missing-test" {} ''
    set -x

    mkdir -p $out/empty-dir
    cd $out/empty-dir
    ${add-missing}/bin/add-missing
    grep -q '^<nixpkgs>$' nixpkgs.nix
    grep -q '^/result' .gitignore

    mkdir -p $out/gitignore-no-result
    cd $out/gitignore-no-result
    printf '/foo\n' >.gitignore
    ${add-missing}/bin/add-missing
    grep -q '^/foo$' .gitignore
    grep -q '^/result$' .gitignore

    set +x
  '';
}
