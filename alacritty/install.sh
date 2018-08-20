#!/bin/bash

set -eo pipefail

if [[ ! -f /Applications/Alacritty.app ]]
then
	mkdir -p ~/src
	cd ~/src
	if [[ -d alacritty ]]; then
		cd alacritty
		git pull --rebase
        else
		git clone git@github.com:jwilm/alacritty.git
		cd alacritty
        fi
	cargo build --release
	make app
	cp -r target/release/osx/Alacritty.app /Applications/
fi
