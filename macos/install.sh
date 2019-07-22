#!/usr/bin/env bash

set -e

[ $(uname -s) != Darwin ] && exit

# Why would I ever want a single-button mouse?
defaults write com.apple.AppleMultitouchMouse MouseButtonMode TwoButton
defaults write com.apple.driver.AppleBluetoothMultitouch.mouse MouseButtonMode TwoButton

# No Command+Space for Spotlight
# This doesn't seem to take effect immediately unless changed through the UI :(
defaults export com.apple.symbolichotkeys /tmp/defaults.plist
plutil -replace AppleSymbolicHotKeys.64.enabled -integer 0 /tmp/defaults.plist
defaults import com.apple.symbolichotkeys /tmp/defaults.plist
