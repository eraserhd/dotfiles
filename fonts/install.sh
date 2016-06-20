#!/bin/bash

if [ "$(uname -s)" = "Darwin" ]
then
  for font in $(dirname $0)/*.ttf
  do
    cp "$font" ~/Library/Fonts/
  done
fi
