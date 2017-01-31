#!/bin/sh

pip install neovim

(
  cd ~/.config/nvim/bundle/node-host/
  npm install
)

# Neovim can't handle multiple plugins right now, so as a hack, we install a
# manually generated rplugin.vim
#/usr/local/bin/nvim --cmd 'au VimEnter * UpdateRemotePlugins' --cmd 'au VimEnter * q'
mkdir -p ~/.local/share/nvim
cp ./neovim/rplugin.vim ~/.local/share/nvim/
