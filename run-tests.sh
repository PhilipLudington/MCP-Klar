#!/bin/bash
# Run all tests for mcp-klar

set -e

cd "$(dirname "$0")"

echo "Running tests..."
zig build test "$@"

echo "All tests passed."
