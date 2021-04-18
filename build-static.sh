#!/usr/bin/env sh
nix-build --arg isJS true -o static
