#!/usr/bin/env nix-shell
#! nix-shell -p gnupg pinentry-curses git-crypt

set -ex

TEMPORARY_CONF=false
if [[ ! -e ~/.gnupg/gpg-agent.conf ]]; then
    TEMPORARY_CONF=true
    printf 'pinentry-program %s\n' "$(which pinentry-curses)" >~/.gnupg/gpg-agent.conf
    gpgconf --kill gpg-agent || true
fi

gpg --import secret.key
git crypt unlock

if $TEMPORARY_CONF; then
    rm -f ~/.gnupg/gpg-agent.conf
fi
