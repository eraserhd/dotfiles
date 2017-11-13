#!/bin/sh

if ! command -v hoogle >/dev/null
then
	set -e
	cabal update
	cabal install hoogle
	hoogle generate
fi
