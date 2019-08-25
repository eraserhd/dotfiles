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

if [[ ! -f default.nix ]]; then
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
fi

if [[ ! -f .gitignore ]] || ! grep -q '^/result$' .gitignore; then
    printf '/result\n' >>.gitignore
fi

if [ ! -f README.* ] && [[ ! -f README ]]; then
    printf '= %s\n' "$packageName" >>README.adoc
fi

if [ ! -f LICENSE* ]; then
  cat >UNLICENSE <<EOF
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org/>
EOF
fi

if [[ ! -f .envrc ]]; then
    printf 'use nix\n' >.envrc
fi
