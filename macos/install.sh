#!/usr/bin/env bash

set -e

[ $(uname -s) != Darwin ] && exit

defaults write com.apple.finder AppleShowAllFiles NO || exit $?

# Keyboard (Needs relogin to take effect)
defaults write -g InitialKeyRepeat -int 20
defaults write -g KeyRepeat -int 1
defaults write -g AppleKeyboardUIMode -int 2               # Tab to all controls
defaults write -g com.apple.keyboard.fnState -boolean true # F-keys are F-keys on external keyboards

# Dock
osascript -e 'tell application "System Events" to set the autohide of the dock preferences to true'

# Why would I ever want a single-button mouse?
defaults write com.apple.AppleMultitouchMouse MouseButtonMode TwoButton
defaults write com.apple.driver.AppleBluetoothMultitouch.mouse MouseButtonMode TwoButton

# No Command+Space for Spotlight
# This doesn't seem to take effect immediately unless changed through the UI :(
defaults export com.apple.symbolichotkeys /tmp/defaults.plist
plutil -replace AppleSymbolicHotKeys.64.enabled -integer 0 /tmp/defaults.plist
defaults import com.apple.symbolichotkeys /tmp/defaults.plist
