#!/bin/sh

pip install neovim
npm install -g neovim

cabal install happy
cabal install ghc-mod

# Not sure this works
nvim -E -s <<EOF
VimProcInstall
UpdateRemotePlugins
q
EOF
