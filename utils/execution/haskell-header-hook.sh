#!/usr/bin/env bash
# Pre-commit hook: auto-append File, Author, Docs, Module header to Haskell files.
# Fails if the corresponding docs file does not exist.
# License: add to generated header when finalised (currently omitted).

set -e

camel2kebab() { echo "$1" | sed 's/\([a-z0-9]\)\([A-Z]\)/\1-\2/g' | tr '[:upper:]' '[:lower:]'; }

to_docs_path() {
    local rel="${1#src/Musikell/}"
    local dir base kebab result

    dir=$(dirname "$rel")
    base=$(basename "$rel" .hs)
    kebab=$(camel2kebab "$base")

    # Build path: each directory component gets kebab-cased and lowered
    # src/Musikell/Core/Types.hs             → docs/reference/core/types.mdx
    # src/Musikell/Runtime/Kernel/Builtin/IO.hs → docs/reference/runtime/kernel/io.mdx
    # src/Musikell/Runtime/Kernel/Builtin.hs → docs/reference/runtime/kernel-builtin.mdx
    # src/Musikell/FFI/Bridge.hs             → docs/reference/ffi/bridge.mdx

    if [[ $dir == "." ]]; then
        # Top-level module under src/Musikell/ (unlikely but handle it)
        echo "docs/reference/${kebab}.mdx"
        return
    fi

    # Convert directory path: lowercase, collapse "Kernel/Builtin" → "kernel"
    result=$(echo "$dir" | tr '[:upper:]' '[:lower:]' | sed 's|/builtin||g')

    echo "docs/reference/${result}/${kebab}.mdx"
}

to_module_name() {
    local rel="${1#src/Musikell/}"
    echo "Musikell.${rel%.hs}" | tr '/' '.'
}

author=$(git config user.email || echo "")
[[ -n $author ]] || {
    echo "ERROR: git config user.email not set"
    exit 1
}

added=$(git diff --cached --name-only --diff-filter=A)
modified=0
for f in "$@"; do
    [[ $f == *.hs ]] || continue
    [[ $f == src/Musikell/* ]] || continue
    echo "$added" | grep -Fxq "$f" || continue

    docs_path=$(to_docs_path "$f")
    if [[ ! -f $docs_path ]]; then
        echo "ERROR: $f requires docs at $docs_path (missing)"
        exit 1
    fi

    if head -5 "$f" | grep -qE '^\s*--\s+File\s*:'; then
        continue
    fi

    module_name=$(to_module_name "$f")
    header="-- File: $f
-- Author: $author
-- Docs: $docs_path
-- Module: $module_name
"
    tmp=$(mktemp)
    printf '%s\n' "$header" >"$tmp"
    cat "$f" >>"$tmp"
    mv "$tmp" "$f"
    git add "$f"
    modified=1
done

if [[ $modified -eq 1 ]]; then
    echo "haskell-header: prepended File/Author/Docs/Module headers"
fi
