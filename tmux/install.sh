#! /bin/sh

set -e

tic tmux/tmux-256color.ncurses57.terminfo
/usr/local/opt/ncurses/bin/tic tmux/tmux-256color.ncurses61.terminfo || true

