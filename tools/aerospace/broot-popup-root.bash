# Prints the directory the broot popup should open in, from the focused window:
# the root of the repository containing the working directory of the focused
# kitty window's foreground process.  Prints nothing, to leave it to the popup,
# when there is no such directory under ~/src.

PATH=$PATH:/run/current-system/sw/bin

focused_kitty_window() {
  local bundle pid
  read -r bundle pid < <(aerospace list-windows --focused --format '%{app-bundle-id} %{app-pid}')
  [ "$bundle" = net.kovidgoyal.kitty ] || return 1
  kitty @ --to "unix:$HOME/.run/kitty-$pid" ls --match state:focused |
    jq -c '[.[].tabs[].windows[] | select(.is_focused)][0] // empty'
}

repository_root() {
  git -C "$1" rev-parse --show-toplevel 2>/dev/null || echo "$1"
}

window=$(focused_kitty_window 2>/dev/null) || exit 0
[ -n "$window" ] || exit 0

dir=$(jq -r '.foreground_processes[0].cwd // .cwd // empty' <<<"$window")

case $dir in
  "$HOME"/src/*) [ -d "$dir" ] && repository_root "$dir" ;;
esac
exit 0
