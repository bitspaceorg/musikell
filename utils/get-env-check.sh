#!/bin/sh
set -e

system=$(nix eval --impure --raw --expr 'builtins.currentSystem')
flake_show_json=$(nix flake show --json --option allow-import-from-derivation true)
checks=$(echo "$flake_show_json" | jq ".checks.\"${system}\" | keys[] | select(test(\"^$1-\"))")
for i in $checks; do echo "checks.${system}.${i}"; done
