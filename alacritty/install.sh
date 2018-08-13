#!/bin/sh

set -e

if [[ ! -f /Applications/Alacritty.app ]]
then
	mkdir -p ~/src
	cd ~/src
	git clone git@github.com:jwilm/alacritty.git
	cd alacritty
	cargo build --release
	make app
	cp -r target/release/osx/Alacritty.app /Applications/
fi
