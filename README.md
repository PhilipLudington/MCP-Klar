# MCP-Klar

A Zig-based MCP (Model Context Protocol) server providing language intelligence for the Klar programming language. This server enables Claude Code to understand, analyze, and navigate Klar codebases.

## Features

- **Type Checking** (`klar_check`) - Validate Klar code and get diagnostics
- **Hover Information** (`klar_hover`) - Get type and documentation info at any position
- **Go to Definition** (`klar_definition`) - Jump to symbol definitions
- **Find References** (`klar_references`) - Find all usages of a symbol
- **Document Symbols** (`klar_symbols`) - List all symbols in a file
- **Code Completion** (`klar_completions`) - Get intelligent code suggestions

## Requirements

- [Zig](https://ziglang.org/) 0.15 or later
- macOS, Linux, or Windows

## Installation

### Building from Source

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/MCP-Klar.git
   cd MCP-Klar
   ```

2. Build the server:
   ```bash
   ./build.sh
   ```

   Or manually:
   ```bash
   zig build -Doptimize=ReleaseSafe
   ```

3. The binary will be at `zig-out/bin/mcp-klar`

### Running Tests

```bash
./run-tests.sh
```

## Configuration

### Environment Variables

| Variable | Description |
|----------|-------------|
| `KLAR_STD_PATH` | Path to the Klar standard library |
| `KLAR_VERBOSE` | Set to `1`, `true`, or `yes` to enable verbose output |

### Project Configuration (klar.json)

Create a `klar.json` file in your project root:

```json
{
    "std_path": "/path/to/klar/std",
    "project_root": ".",
    "source_dirs": ["src", "lib"],
    "verbose": false
}
```

Configuration precedence (highest to lowest):
1. Environment variables
2. `klar.json` file
3. Default values

## Usage with Claude Code

Add the server to your Claude Code MCP configuration:

```json
{
    "mcpServers": {
        "klar": {
            "command": "/path/to/mcp-klar"
        }
    }
}
```

## Available Tools

### klar_check

Type check a Klar file and return diagnostics.

**Parameters:**
- `file` (string, required) - Path to the .kl file
- `content` (string, optional) - File content override for unsaved buffers

### klar_hover

Get type and documentation information for a symbol at a position.

**Parameters:**
- `file` (string, required) - Path to the .kl file
- `line` (integer, required) - Line number (1-indexed)
- `column` (integer, required) - Column number (1-indexed)
- `content` (string, optional) - File content override for unsaved buffers

### klar_definition

Go to the definition of a symbol at a position.

**Parameters:**
- `file` (string, required) - Path to the .kl file
- `line` (integer, required) - Line number (1-indexed)
- `column` (integer, required) - Column number (1-indexed)
- `content` (string, optional) - File content override for unsaved buffers

### klar_references

Find all references to a symbol at a position.

**Parameters:**
- `file` (string, required) - Path to the .kl file
- `line` (integer, required) - Line number (1-indexed)
- `column` (integer, required) - Column number (1-indexed)
- `content` (string, optional) - File content override for unsaved buffers
- `include_definition` (boolean, optional) - Include the definition in results

### klar_symbols

List all symbols in a Klar file.

**Parameters:**
- `file` (string, required) - Path to the .kl file
- `content` (string, optional) - File content override for unsaved buffers
- `kind` (string, optional) - Filter by symbol kind (function, struct, enum, variable, etc.)

### klar_completions

Get code completions at a position in a Klar file.

**Parameters:**
- `file` (string, required) - Path to the .kl file
- `line` (integer, required) - Line number (1-indexed)
- `column` (integer, required) - Column number (1-indexed)
- `content` (string, optional) - File content override for unsaved buffers
- `prefix` (string, optional) - Prefix to filter completions

## Available Resources

### klar://std/docs

Returns comprehensive documentation for the Klar standard library.

### klar://project/structure

Returns the project directory structure, listing all `.kl` files.

## Troubleshooting

### Server doesn't start

1. Ensure Zig 0.15+ is installed: `zig version`
2. Rebuild from source: `./build.sh`
3. Check that the binary exists: `ls -la zig-out/bin/mcp-klar`

### No diagnostics returned

1. Ensure the file path is correct and accessible
2. Check that the file has a `.kl` extension
3. Try using the `content` parameter with the file contents

### Standard library not found

Set the `KLAR_STD_PATH` environment variable or configure it in `klar.json`.

## License

MIT
