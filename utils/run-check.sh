#!/bin/sh
set -e

checks=$(./utils/get-env-check.sh "${1:-dev}")
nix build --no-link $(echo .#$checks | sed 's/ / .#/g')
