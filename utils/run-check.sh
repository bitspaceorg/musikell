#!/bin/sh
set -e

checks=$(./utils/get-env-check.sh "${1:-dev}")
nix build --print-build-logs $(echo .#$checks | sed 's/ / .#/g')
