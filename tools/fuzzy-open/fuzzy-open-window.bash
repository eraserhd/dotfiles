# Picks a file with fzf, and plumbs it.  Given a directory, it picks from the
# files in it; otherwise it picks a repository under ~/src first.  What
# fre-pluggo says was visited most is listed first.

src=$HOME/src

# Repositories, as "path<TAB>name", where the name leaves out the host.
repositories() {
  {
    fre-pluggo list -p "$src/" 2>/dev/null |
      sed -nE "s|^($src/[^/]+/[^/]+/[^/]+).*|\1|p"
    printf '%s\n' "$src"/*/*/*/ | sed 's|/$||'
  } |
    awk '!seen[$0]++' |
    while read -r dir; do
      [ -d "$dir" ] && printf '%s\t%s\n' "$dir" "$(name "$dir")"
    done
}

# What to call a directory: under ~/src, without the host.
name() {
  case $1 in
    "$src"/*) local rel=${1#"$src"/}; printf '%s\n' "${rel#*/}" ;;
    "$HOME"/*) printf '%s/%s\n' '~' "${1#"$HOME"/}" ;;
    *) printf '%s\n' "$1" ;;
  esac
}

# Files under dir, relative to it.
files() {
  local dir=$1
  {
    fre-pluggo list -p "$dir/" 2>/dev/null |
      while read -r file; do
        [ -f "$file" ] && printf '%s\n' "${file#"$dir"/}"
      done
    if git -C "$dir" rev-parse >/dev/null 2>&1; then
      git -C "$dir" ls-files --cached --others --exclude-standard
    else
      (cd "$dir" && rg --files --hidden --glob '!.git')
    fi
  } | awk '!seen[$0]++'
}

pick_repository() {
  repositories |
    fzf --scheme=history --delimiter='\t' --with-nth=2 --prompt='repository> ' |
    cut -f1
}

dir=${1:-}
while :; do
  if [ -z "$dir" ]; then
    dir=$(pick_repository)
    [ -n "$dir" ] || exit 0
  fi
  picked=$(files "$dir" |
    fzf --scheme=history --expect=ctrl-o --header='C-o: another repository' \
        --prompt="$(name "$dir")> ") || exit 0
  key=$(sed -n 1p <<<"$picked")
  file=$(sed -n 2p <<<"$picked")
  if [ "$key" = ctrl-o ]; then
    dir=
    continue
  fi
  exec 9 plumb "$dir/$file"
done
