#!/bin/bash

[ "$(uname -s)" != "Darwin" ] && exit

defaults write com.apple.finder AppleShowAllFiles NO || exit $?

# Dock
defaults write com.apple.dock autohide -bool true

# Why would I ever want a single-button mouse?
defaults write com.apple.AppleMultitouchMouse MouseButtonMode TwoButton
defaults write com.apple.driver.AppleBluetoothMultitouch.mouse MouseButtonMode TwoButton

# Terminal.app
defaults write com.apple.Terminal "Default Window Settings" Pro
