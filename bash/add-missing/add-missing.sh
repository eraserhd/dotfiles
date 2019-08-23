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

if [[ ! -f .gitignore ]] || ! grep -q '^/result$' .gitignore; then
    printf '/result\n' >>.gitignore
fi

if [ ! -f README.* ] && [[ ! -f README ]]; then
    printf '= %s\n' "$packageName" >>README.adoc
fi

if [[ ! -f .envrc ]]; then
    printf 'use nix\n' >.envrc
fi
