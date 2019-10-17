#!/usr/bin/env bash

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/scripts"

tmux bind -Tcopy-mode-vi Enter run-shell $SCRIPTS/plumb
