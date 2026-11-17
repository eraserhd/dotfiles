# Picks a file under ~/src with fzf, and plumbs it.  What fre-pluggo says was
# visited most is listed first.

src=$HOME/src
cd "$src"

# Files, relative to ~/src.
files() {
  fre-pluggo list -p "$src/" 2>/dev/null |
    while read -r file; do
      [ -f "$file" ] && printf '%s\n' "${file#"$src"/}"
    done
  rg --files --hidden --glob '!.git'
}

# Hosts are left out of what's shown and matched.
file=$(files |
  awk '!seen[$0]++' |
  fzf --scheme=history --delimiter=/ --with-nth=2.. --prompt='src> ') || exit 0
exec 9 plumb "$src/$file"
