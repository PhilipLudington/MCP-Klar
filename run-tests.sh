#!/bin/bash
# Run all tests for mcp-klar and write results to .test-results.json

set -e

cd "$(dirname "$0")"

echo "Running tests..."

# Capture test output
TEST_OUTPUT=$(zig build test 2>&1) || {
    # Tests failed
    echo "$TEST_OUTPUT"

    # Try to parse failure count from output
    FAILED=$(echo "$TEST_OUTPUT" | grep -oE '[0-9]+ failed' | grep -oE '[0-9]+' || echo "1")
    PASSED=$(echo "$TEST_OUTPUT" | grep -oE '[0-9]+/[0-9]+ tests passed' | grep -oE '^[0-9]+' || echo "0")
    TOTAL=$((PASSED + FAILED))

    cat > .test-results.json << EOF
{
  "passed": $PASSED,
  "failed": $FAILED,
  "total": $TOTAL,
  "failures": ["See test output for details"]
}
EOF
    echo "Tests failed."
    exit 1
}

echo "$TEST_OUTPUT"

# Parse passed test count from Zig output
# Zig test output shows "X/Y tests passed" or just succeeds silently
PASSED=$(echo "$TEST_OUTPUT" | grep -oE '[0-9]+/[0-9]+ tests passed' | grep -oE '^[0-9]+' || echo "")

if [ -z "$PASSED" ]; then
    # No test count in output - count test declarations in source
    TOTAL=$(grep -r "^test " src/ | wc -l | tr -d ' ')
    PASSED=$TOTAL
else
    TOTAL=$(echo "$TEST_OUTPUT" | grep -oE '[0-9]+/[0-9]+ tests passed' | grep -oE '/[0-9]+' | tr -d '/')
fi

cat > .test-results.json << EOF
{
  "passed": $PASSED,
  "failed": 0,
  "total": $TOTAL,
  "failures": []
}
EOF

echo "All tests passed. ($PASSED/$TOTAL)"
