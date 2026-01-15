#!/bin/bash
# Build the mcp-klar server and write results to .build-results.json

cd "$(dirname "$0")"

echo "Building mcp-klar..."

# Capture build output
BUILD_OUTPUT=$(zig build 2>&1)
BUILD_EXIT=$?

if [ $BUILD_EXIT -eq 0 ]; then
    # Count warnings if any
    WARNINGS=$(echo "$BUILD_OUTPUT" | grep -c "warning:" 2>/dev/null || true)
    WARNINGS=${WARNINGS:-0}

    cat > .build-results.json << EOF
{
  "success": true,
  "errors": 0,
  "warnings": $WARNINGS,
  "messages": []
}
EOF
    echo "$BUILD_OUTPUT"
    echo "Build complete. Binary at: zig-out/bin/mcp-klar"
else
    # Build failed
    ERRORS=$(echo "$BUILD_OUTPUT" | grep -c "error:" 2>/dev/null || true)
    ERRORS=${ERRORS:-1}
    WARNINGS=$(echo "$BUILD_OUTPUT" | grep -c "warning:" 2>/dev/null || true)
    WARNINGS=${WARNINGS:-0}

    # Extract error messages
    ERROR_MSGS=$(echo "$BUILD_OUTPUT" | grep "error:" | head -5 | sed 's/"/\\"/g' | awk '{printf "\"%s\",", $0}' | sed 's/,$//')

    cat > .build-results.json << EOF
{
  "success": false,
  "errors": $ERRORS,
  "warnings": $WARNINGS,
  "messages": [$ERROR_MSGS]
}
EOF
    echo "$BUILD_OUTPUT"
    echo "Build failed."
    exit 1
fi
