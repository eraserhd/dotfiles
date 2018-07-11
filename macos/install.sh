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
defaults write com.apple.Terminal AppleShowScrollBars -string WhenScrolling

# No Command+Space for Spotlight
# This doesn't seem to take effect immediately unless changed through the UI :(
defaults export com.apple.symbolichotkeys /tmp/defaults.plist
plutil -replace AppleSymbolicHotKeys.64.enabled -integer 0 /tmp/defaults.plist
defaults import com.apple.symbolichotkeys /tmp/defaults.plist
