#!@bash@/bin/bash

set -e

packageName=$(basename "$(pwd)")

if [[ ! -f nixpkgs.nix ]]; then
    printf '<nixpkgs>\n' >nixpkgs.nix
fi

if [[ ! -f overlay.nix ]]; then
  (
    printf 'self: super: {\n'
    printf '  %s = super.callPackage ./derivation.nix {};\n' "$packageName"
    printf '}\n'
  ) >overlay.nix
fi

(
  printf 'let\n'
  printf '  nixpkgs = import ./nixpkgs.nix;\n'
  printf '  pkgs = import nixpkgs {\n'
  printf '    config = {};\n'
  printf '    overlays = [ (import ./overlay.nix) ];\n'
  printf '  };\n'
  printf '\n'
  printf 'in pkgs.%s\n' "$packageName"
) >default.nix

if [[ ! -f .gitignore ]] || ! grep -q '^/result$' .gitignore; then
    printf '/result\n' >>.gitignore
fi

if [ ! -f README.* ] && [[ ! -f README ]]; then
    printf '= %s\n' "$packageName" >>README.adoc
fi

if [[ ! -f .envrc ]]; then
    printf 'use nix\n' >.envrc
fi
