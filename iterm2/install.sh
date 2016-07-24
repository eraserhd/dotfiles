#!/bin/bash

[ "$(uname -s)" == "Darwin" ] || exit

set -e

# Load settings from our config folder
defaults write com.googlecode.iterm2 PrefsCustomFolder ~/.config/iterm2
defaults write com.googlecode.iterm2 LoadPrefsFromCustomFolder 1
