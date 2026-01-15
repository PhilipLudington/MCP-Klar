#!/bin/bash
# Build the mcp-klar server

set -e

cd "$(dirname "$0")"

echo "Building mcp-klar..."
zig build "$@"

echo "Build complete. Binary at: zig-out/bin/mcp-klar"
