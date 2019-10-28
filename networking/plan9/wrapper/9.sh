#!@bash@/bin/bash

set -e

: ${XDG_RUNTIME_DIR:=~/.run}
: ${NAMESPACE:=$XDG_RUNTIME_DIR/plan9/srv}

export NAMESPACE
mkdir -p $NAMESPACE
exec @plan9port@/bin/9 "$@"
