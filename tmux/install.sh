#! /bin/sh

set -e

./tmux/tmux.symlink/plugins/tpm/bin/install_plugins

tic tmux/tmux-256color.ncurses57.terminfo
/usr/local/opt/ncurses/bin/tic tmux/tmux-256color.ncurses61.terminfo || true

