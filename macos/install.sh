#!/usr/bin/env bash

set -e

[ $(uname -s) != Darwin ] && exit

# Why would I ever want a single-button mouse?
defaults write com.apple.AppleMultitouchMouse MouseButtonMode TwoButton
defaults write com.apple.driver.AppleBluetoothMultitouch.mouse MouseButtonMode TwoButton
