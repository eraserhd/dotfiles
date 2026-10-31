# Prints the directory the broot popup should open in, from the focused window:
# the root of the repository containing the file which the focused Kakoune
# client is showing, or else the working directory of the focused kitty
# window's foreground process.  Prints nothing, to leave it to the popup, when
# there is no such directory under ~/src.

PATH=$PATH:/run/current-system/sw/bin

focused_kitty_window() {
  local bundle pid
  read -r bundle pid < <(aerospace list-windows --focused --format '%{app-bundle-id} %{app-pid}')
  [ "$bundle" = net.kovidgoyal.kitty ] || return 1
  kitty @ --to "unix:$HOME/.run/kitty-$pid" ls --match state:focused |
    jq -c '[.[].tabs[].windows[] | select(.is_focused)][0] // empty'
}

# The file shown by the Kakoune client with a pid.  The server answers
# asynchronously, each client writing a file named for its pid.
kakoune_file() {
  local session=$1 pid=$2 results file=
  results=$(mktemp -d)
  kak -p "$session" <<EOF
evaluate-commands %sh{
  for client in \$kak_client_list; do
    echo "evaluate-commands -client \$client %{ echo -to-file \"$results/%val{client_pid}\" %val{buffile} }"
  done
}
EOF
  for _ in 1 2 3 4 5 6 7 8 9 10; do
    [ -e "$results/$pid" ] && break
    sleep 0.1
  done
  [ -e "$results/$pid" ] && file=$(<"$results/$pid")
  rm -rf "$results"
  echo "$file"
}

repository_root() {
  git -C "$1" rev-parse --show-toplevel 2>/dev/null || echo "$1"
}

window=$(focused_kitty_window 2>/dev/null) || exit 0
[ -n "$window" ] || exit 0

dir=
read -r kak_pid kak_session < <(jq -r '
  [.foreground_processes[]
   | select(.cmdline[0] | test("(^|/)kak$"))
   | select(.cmdline | index("-c"))
   | "\(.pid) \(.cmdline[(.cmdline | index("-c")) + 1])"][0] // empty' <<<"$window") || true
if [ -n "${kak_pid:-}" ]; then
  file=$(kakoune_file "$kak_session" "$kak_pid")
  [ -f "$file" ] && dir=$(dirname "$file")
fi
[ -n "$dir" ] || dir=$(jq -r '.foreground_processes[0].cwd // .cwd // empty' <<<"$window")

case $dir in
  "$HOME"/src/*) [ -d "$dir" ] && repository_root "$dir" ;;
esac
exit 0
