#!/usr/bin/env bash

export PATH="@reattach-to-user-namespace@/bin:$PATH"

openInChromeTab() {
    local link="$1"
    local frontmost_pid=$(reattach-to-user-namespace lsappinfo info -only pid $(reattach-to-user-namespace lsappinfo front))
    frontmost_pid="${frontmost_pid##*=}"
    reattach-to-user-namespace osascript -e "
        on run argv
            set theUrl to item 1 of argv
            set theFrontmostPid to item 2 of argv
            set found to false
            tell application \"Google Chrome\"
                repeat with theWindow in every window
                    set tabIndex to 1
                    repeat with theTab in every tab of theWindow
                        if theTab's URL is theUrl then
                            set found to true
                            exit repeat
                        end if
                        set tabIndex to tabIndex + 1
                    end repeat
                    if found then
                        exit repeat
                    end if
                end repeat
                if found then
                    set theWindow's active tab index to tabIndex
                    set index of theWindow to 1
                else
                    tell window 1 to make new tab with properties {URL:theUrl}
                end if
            end tell
            tell application \"System Events\"
                set frontmost of every process whose unix id is theFrontmostPid to true
            end tell
        end run
    " "$link" "$frontmost_pid"
}

openInChromeTab "$1"
