#!/bin/sh

for file in ./ssh/files/*
do
	cp "$file" "~/.ssh/$file"
done
chmod 600 ~/.ssh/id_rsa ~/.ssh/id_dsa
