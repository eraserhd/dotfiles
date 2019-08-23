#!@bash@/bin/bash

set -e

packageName=$(basename "$(pwd)")

if [[ ! -f nixpkgs.nix ]]; then
    printf '<nixpkgs>\n' >nixpkgs.nix
fi

if [[ ! -f .gitignore ]] || ! grep -q '^/result$' .gitignore; then
    printf '/result\n' >>.gitignore
fi

if [ ! -f README.* ] && [[ ! -f README ]]; then
    printf '= %s\n' "$packageName" >>README.adoc
fi

printf 'use nix\n' >.envrc
