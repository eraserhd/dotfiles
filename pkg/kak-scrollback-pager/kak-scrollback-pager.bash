#!@bash@/bin/bash
#
# This works with kak-ansi. It might work without if `--stdin-add-formatting`
# is removed below.
#
# Example kitty config:
#     scrollback_lines 5000
#     map shift+ctrl+h launch --stdin-add-formatting --stdin-source=@screen_scrollback --cwd=current --type=overlay kak-scrollback-pager @scrolled-by @cursor-x @cursor-y @line-count
#

scrolledBy="$1"
cursorX="$2"
cursorY="$3"
lines="$4"

# select must happen in the window context, but we are notified of new
# content in the buffer context.  For scrollback buffers, we assume there's
# only one client, and the user does not change the buffer in it.
exec @kakoune@/bin/kak -e '
    try %{ declare-option str kak_scrollback_pager_client }
    set-option buffer kak_scrollback_pager_client %val{client}
    hook -group kak-scrollback-pager buffer BufReadFifo .* %{
        evaluate-commands -client %opt{kak_scrollback_pager_client} %sh{
            cursorLine=$(( kak_buf_line_count - '"$scrolledBy"' - '"$lines"' + '"$cursorY"' ))
            printf "select %d.%d,%d.%d\n" "$cursorLine" "'"$cursorX"'" "$cursorLine" "'"$cursorX"'"
            printf "execute-keys vt\n"
            if [ '"$cursorY"' -gt 2 ]; then
                printf "execute-keys %dvk\n" $(( '"$cursorY"' - 2 ))
            fi
        }
    }
    map window normal <esc> ": delete-buffer! ; quit<ret>"
    try %{ set-option buffer plumb_wdir %{'"$(pwd)"'} }
'
