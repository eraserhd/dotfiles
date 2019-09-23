#!@bash@/bin/bash

set -e

export PATH="@git@/bin:$PATH"

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

if [[ ! -f derivation.nix ]]; then
  (
    printf '{ stdenv, fetchFromGitHub, ... }:\n'
    printf '\n'
    printf 'stdenv.mkDerivation rec {\n'
    printf '  pname = "%s";\n' "$packageName"
    printf '  version = "0.1.0";\n'
    printf '\n'
    printf '  src = ./.;\n'
    printf '\n'
    printf '  meta = with stdenv.lib; {\n'
    printf '    description = "TODO: fill me in";\n'
    printf '    homepage = https://github.com/eraserhd/%s;\n' "$packageName"
    printf '    license = licenses.publicDomain;\n'
    printf '    platforms = platforms.all;\n'
    printf '    maintainers = [ maintainers.eraserhd ];\n'
    printf '  };\n'
    printf '}\n'
  ) >derivation.nix
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

if [[ ! -f release.nix ]]; then
  (
    printf '{ nixpkgs ? (import ./nixpkgs.nix), ... }:\n'
    printf 'let\n'
    printf '  pkgs = import nixpkgs { config = {}; };\n'
    printf '  %s = pkgs.callPackage ./derivation.nix {};\n' "$packageName"
    printf 'in {\n'
    printf '  test = pkgs.runCommandNoCC "%s-test" {} %s\n' "$packageName" "''"
    printf '    mkdir -p $out\n'
    printf '    : ${%s}\n' "$packageName"
    printf '  %s;\n' "''"
    printf '}'
  ) >release.nix
fi

if [[ ! -f .gitignore ]] || ! grep -q '^/result$' .gitignore; then
    printf '/result\n' >>.gitignore
fi

if [ ! -f README.* ] && [[ ! -f README ]]; then
    printf '= %s\n' "$packageName" >>README.adoc
fi

if [ ! -f LICENSE* ] && [ ! -f COPYING* ]; then
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

if [ ! -f ChangeLog* ] && [ ! -f CHANGELOG* ]; then
  (
    printf '= Changes\n'
    printf '\n'
    printf '== Unreleased\n'
    printf '\n'
  ) >CHANGELOG.adoc
fi

if [[ ! -f .envrc ]]; then
    printf 'use nix\n' >.envrc
fi

if [[ ! -d $(git rev-parse --git-dir 2>/dev/null) ]]; then
  git init
fi

if ! git remote |grep -q '^origin$'; then
  git remote add origin git@github.com:eraserhd/$packageName.git
fi

firstCommit=false
if ! git log >/dev/null 2>&1; then
    firstCommit=true
fi

if [[ -n $(git status --porcelain) ]]; then
    git add -A
    if $firstCommit; then
        git commit -m "Initial commit"
        git checkout -b develop
        git branch -D master
    else
        git commit -m "Update project structure"
    fi
fi
