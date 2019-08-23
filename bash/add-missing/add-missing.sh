#!@bash@/bin/bash

if ! [[ -f nixpkgs.nix ]]; then
    printf 'import <nixpkgs>\n' >nixpkgs.nix
fi
