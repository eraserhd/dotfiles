#!@bash@/bin/bash

if ! [[ -f nixpkgs.nix ]]; then
    printf '<nixpkgs>\n' >nixpkgs.nix
fi

if ! [[ -f .gitignore ]]; then
    printf '/result\n' >.gitignore
fi
