#!/bin/bash

[ "$(uname -s)" != "Darwin" ] || exit

defaults write com.apple.finder AppleShowAllFiles NO || exit $?
