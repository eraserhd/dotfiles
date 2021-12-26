#!/usr/bin/env bash

set -eo pipefail

mkdir -p ~/src
while read repo_url; do
  dir=${repo_url##*/}  
  dir=${dir%.git}
  if [[ ! -d ~/src/"$dir" ]]; then
    ( cd ~/src ; git clone "$repo_url" )
  fi
done <programs/git/repos.txt
