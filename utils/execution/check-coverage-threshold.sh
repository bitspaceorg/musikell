#!/usr/bin/env sh
# Parse HPC report output and fail if coverage < threshold (default 80%).

set -e

threshold="${1:-80}"
report="${2:-}"

if [ -z "$report" ]; then
    report=$(cat)
elif [ -f "$report" ]; then
    report=$(cat "$report")
fi

pct=$(echo "$report" | sed -n 's/.*\([0-9][0-9]*\.[0-9]*\)% expressions used.*/\1/p' | head -1)
if [ -z "$pct" ]; then
    echo "Could not parse coverage percentage from report"
    exit 1
fi

# Compare using awk for float
ok=$(echo "$pct $threshold" | awk '{print ($1 >= $2)}')
if [ "$ok" = "0" ]; then
    echo "Coverage ${pct}% is below threshold ${threshold}%"
    exit 1
fi
echo "Coverage ${pct}% meets threshold ${threshold}%"
