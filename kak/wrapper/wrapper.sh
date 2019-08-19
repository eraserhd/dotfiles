#!@bash@/bin/bash
#
# kak - A wrapper for Kakoune which connects to (or creates) a session named
#       for the current tmux window.
#

kak="@kakoune@/bin/kak"
declare -a extraArgs=()

canSpecifySession() {
    while (( $# != 0 )); do
        case "$1" in
            -l|-c|-s|-n) return 1;;
            -e|-E|-p|-f|-i|-ui|-debug) shift;;
        esac
        shift
    done
    return 0
}

findSessionName() {
    if [[ -n $TMUX_PANE ]]; then
        tmux display -t "$TMUX_PANE" -p '#W'
        return $?
    fi
    printf 'kakoune\n'
}

sessionExists() {
    local session="$1"
    if "$kak" -l 2>/dev/null |grep -q "${session}\$"; then
        return 0
    else
        return 1
    fi
}

addSessionArgs() {
    if ! canSpecifySession "$@"; then
        return
    fi
    local session="$(findSessionName)"
    if sessionExists "$session"; then
        extraArgs+=( -c "$session" )
    else
        extraArgs+=( -s "$session" )
    fi
}

addSessionArgs "$@"
exec "$kak" "${extraArgs[@]}" "$@"
