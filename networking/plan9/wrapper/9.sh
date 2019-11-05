#!@bash@/bin/bash

: ${XDG_RUNTIME_DIR:=~/.run}
: ${NAMESPACE:=$XDG_RUNTIME_DIR/plan9/srv}

export NAMESPACE
mkdir -p $NAMESPACE 2>/dev/null
exec @plan9port@/bin/9 "$@"
