# MCP-Klar Project Instructions

CarbideZig is used in this project for Zig development standards. See `carbide/CARBIDE.md` and `carbide/STANDARDS.md`.

## Running Tests

Always use the GitStat wrapper script to run tests:
```bash
./run-tests.sh
```
Do NOT run `zig build test` directly - use the wrapper script to preserve GitStat integration and result tracking.

## Building

Always use the GitStat wrapper script to build:
```bash
./build.sh
```
Do NOT run `zig build` directly - use the wrapper script to preserve GitStat integration.
