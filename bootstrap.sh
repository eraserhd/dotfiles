#!/usr/bin/env bash

set -ex

#Install nix
#Install homebrew

brew cask install 1password

nix-env -i gnupg
nix-env -i git-crypt

git crypt unlock


sed -i '' -e 's,git@github\.com:,https://github.com/,' .gitmodules
git submodule init
git submodule update

# 2u-nix asks for password, fails




