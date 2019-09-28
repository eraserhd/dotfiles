#!/bin/sh

mkdir -p ~/.ssh
chmod 700 ~/.ssh

cp -ap ./ssh/files/* ~/.ssh/
chmod 600 ~/.ssh/id_*
