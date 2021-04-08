#!/usr/bin/env bash
docker load < $(nix-build --arg docker true)
