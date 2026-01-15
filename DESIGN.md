# Klar MCP Server Design Document

> **Language tooling for Claude Code via Model Context Protocol**

A Zig-based MCP server that provides Klar language intelligence to Claude Code, enabling real-time diagnostics, type information, and code navigation.

---

## Table of Contents

1. [Overview](#overview)
2. [Goals](#goals)
3. [MCP Protocol](#mcp-protocol)
4. [Tools](#tools)
5. [Resources](#resources)
6. [Architecture](#architecture)
7. [Data Structures](#data-structures)
8. [Implementation Plan](#implementation-plan)
9. [Configuration](#configuration)
10. [Future Work](#future-work)

---

## Overview

The Klar MCP Server bridges the Klar compiler with Claude Code, exposing language analysis capabilities through the Model Context Protocol. This allows Claude Code to:

- Validate Klar code before suggesting it
- Understand types and signatures in existing code
- Navigate codebases accurately
- Provide contextually correct completions

### Why MCP?

MCP (Model Context Protocol) is Claude Code's native extension mechanism. Unlike LSP which targets editors, MCP targets AI assistants directly. This means:

- Direct integration with Claude Code's tool system
- No editor middleware required
- Optimized for AI-assisted development workflows

---

## Goals

### Primary Goals

1. **Diagnostics** — Report syntax and type errors with precise locations
2. **Type Information** — Provide type signatures for any symbol
3. **Navigation** — Find definitions and references
4. **Validation** — Check code correctness before file writes

### Non-Goals (Phase 1)

- Code formatting (use `klar fmt` CLI)
- Refactoring operations
- Project-wide analysis
- Debugging support

---

## MCP Protocol

### Transport

The server uses **stdio transport** — communication over stdin/stdout with JSON-RPC 2.0 messages.

```
Claude Code <--stdin/stdout--> mcp-klar
```

### Message Format

```json
// Request
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "klar_check",
    "arguments": { "file": "src/main.kl" }
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [
      { "type": "text", "text": "..." }
    ]
  }
}
```

### Lifecycle

1. Claude Code spawns `mcp-klar` process
2. Server sends `initialize` capabilities
3. Claude Code calls tools as needed
4. Server responds with results
5. Process stays alive for session duration

---

## Tools

### klar_check

Type check a file and return diagnostics.

**Input:**
```json
{
  "file": "string (required) — Path to .kl file",
  "content": "string (optional) — File content override (for unsaved buffers)"
}
```

**Output:**
```json
{
  "success": true,
  "diagnostics": [
    {
      "severity": "error|warning|info",
      "message": "Type mismatch: expected i32, found string",
      "file": "src/main.kl",
      "line": 15,
      "column": 10,
      "end_line": 15,
      "end_column": 22
    }
  ],
  "summary": "2 errors, 1 warning"
}
```

**Use Case:** Claude validates generated code before writing it.

---

### klar_hover

Get type information at a specific position.

**Input:**
```json
{
  "file": "string — Path to .kl file",
  "line": "number — 1-indexed line",
  "column": "number — 1-indexed column",
  "content": "string (optional) — File content override"
}
```

**Output:**
```json
{
  "found": true,
  "kind": "function|variable|type|field|parameter",
  "name": "process_item",
  "type": "fn(item: Item) -> Result[Output, Error]",
  "documentation": "Processes a single item from the queue.\n\nReturns the processed output or an error.",
  "defined_in": "src/processor.kl:42"
}
```

**Use Case:** Claude understands what existing code does before modifying it.

---

### klar_goto_definition

Find where a symbol is defined.

**Input:**
```json
{
  "file": "string — Path to .kl file",
  "line": "number — 1-indexed line",
  "column": "number — 1-indexed column"
}
```

**Output:**
```json
{
  "found": true,
  "definitions": [
    {
      "file": "src/types.kl",
      "line": 28,
      "column": 1,
      "kind": "struct",
      "preview": "pub struct User {"
    }
  ]
}
```

**Use Case:** Claude locates the actual definition to read/modify it.

---

### klar_references

Find all usages of a symbol.

**Input:**
```json
{
  "file": "string — Path to .kl file",
  "line": "number — 1-indexed line",
  "column": "number — 1-indexed column",
  "include_definition": "boolean (default: false)"
}
```

**Output:**
```json
{
  "symbol": "User",
  "kind": "struct",
  "references": [
    {
      "file": "src/main.kl",
      "line": 15,
      "column": 12,
      "context": "let user: User = ..."
    },
    {
      "file": "src/api.kl",
      "line": 42,
      "column": 25,
      "context": "fn get_user(...) -> User"
    }
  ],
  "count": 2
}
```

**Use Case:** Claude understands impact before renaming or changing a type.

---

### klar_completions

Get completion suggestions at a position.

**Input:**
```json
{
  "file": "string — Path to .kl file",
  "line": "number — 1-indexed line",
  "column": "number — 1-indexed column",
  "content": "string (optional) — File content override"
}
```

**Output:**
```json
{
  "completions": [
    {
      "label": "push",
      "kind": "method",
      "type": "fn(self: &mut Self, item: T)",
      "documentation": "Appends an item to the end of the list.",
      "insert_text": "push($1)"
    },
    {
      "label": "pop",
      "kind": "method",
      "type": "fn(self: &mut Self) -> Option[T]",
      "documentation": "Removes and returns the last item."
    }
  ],
  "context": {
    "receiver_type": "List[i32]",
    "trigger": "."
  }
}
```

**Use Case:** Claude suggests valid method calls and field accesses.

---

### klar_symbols

List all symbols in a file or project.

**Input:**
```json
{
  "file": "string (optional) — Specific file, or omit for project",
  "kind": "string (optional) — Filter: function|struct|enum|trait|const"
}
```

**Output:**
```json
{
  "symbols": [
    {
      "name": "main",
      "kind": "function",
      "type": "fn() -> void",
      "file": "src/main.kl",
      "line": 5,
      "visibility": "private"
    },
    {
      "name": "User",
      "kind": "struct",
      "file": "src/types.kl",
      "line": 12,
      "visibility": "public",
      "children": [
        { "name": "name", "kind": "field", "type": "String" },
        { "name": "email", "kind": "field", "type": "String" }
      ]
    }
  ]
}
```

**Use Case:** Claude explores codebase structure.

---

## Resources

MCP resources provide read-only data that Claude can reference.

### klar://std/docs

Standard library documentation.

```json
{
  "uri": "klar://std/docs/collections/List",
  "mimeType": "text/markdown",
  "content": "# List[T]\n\nA growable array type.\n\n## Methods\n\n### push\n```klar\nfn push(self: &mut Self, item: T)\n```\n..."
}
```

### klar://project/structure

Project file tree and module structure.

```json
{
  "uri": "klar://project/structure",
  "mimeType": "application/json",
  "content": {
    "name": "myproject",
    "modules": [
      { "name": "main", "file": "src/main.kl" },
      { "name": "utils", "file": "src/utils.kl" }
    ]
  }
}
```

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                       Claude Code                            │
└─────────────────────────┬───────────────────────────────────┘
                          │ stdio (JSON-RPC)
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                     mcp-klar (Zig)                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │ MCP Server  │──│   Router    │──│   Tool Handlers     │  │
│  │ (JSON-RPC)  │  │             │  │                     │  │
│  └─────────────┘  └─────────────┘  └──────────┬──────────┘  │
│                                               │              │
│  ┌────────────────────────────────────────────▼───────────┐ │
│  │                  Compiler Frontend                      │ │
│  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌───────────┐  │ │
│  │  │  Lexer  │──│  Parser │──│  Types  │──│  Checker  │  │ │
│  │  └─────────┘  └─────────┘  └─────────┘  └───────────┘  │ │
│  └────────────────────────────────────────────────────────┘ │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │                   Analysis Layer                        │ │
│  │  ┌───────────┐  ┌───────────┐  ┌────────────────────┐  │ │
│  │  │ Symbol    │  │ Reference │  │ Completion         │  │ │
│  │  │ Table     │  │ Index     │  │ Engine             │  │ │
│  │  └───────────┘  └───────────┘  └────────────────────┘  │ │
│  └────────────────────────────────────────────────────────┘ │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │                   Document Cache                        │ │
│  │  (In-memory storage for open files + parse results)     │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### Components

**MCP Server**
- Handles JSON-RPC protocol
- Parses requests, serializes responses
- Manages connection lifecycle

**Router**
- Dispatches tool calls to handlers
- Validates input parameters
- Formats output responses

**Tool Handlers**
- Implement individual tool logic
- Coordinate with compiler frontend
- Return structured results

**Compiler Frontend**
- Lexer, Parser, Type Checker from Klar compiler
- Shared with CLI compiler
- Produces AST and type information

**Analysis Layer**
- Symbol Table: Maps names to definitions
- Reference Index: Tracks all usages of each symbol
- Completion Engine: Generates context-aware suggestions

**Document Cache**
- Stores file contents and parsed ASTs
- Invalidates on file changes
- Enables fast repeated queries

---

## Data Structures

### Position

```zig
pub const Position = struct {
    line: u32,      // 1-indexed
    column: u32,    // 1-indexed (UTF-8 bytes)
    offset: usize,  // 0-indexed byte offset
};
```

### Span

```zig
pub const Span = struct {
    start: Position,
    end: Position,
    file_id: u32,
};
```

### Diagnostic

```zig
pub const Diagnostic = struct {
    severity: Severity,
    message: []const u8,
    span: Span,
    code: ?[]const u8,       // e.g., "E0001"
    hints: []Hint,

    pub const Severity = enum {
        error,
        warning,
        info,
        hint,
    };

    pub const Hint = struct {
        message: []const u8,
        span: ?Span,
    };
};
```

### Symbol

```zig
pub const Symbol = struct {
    name: []const u8,
    kind: Kind,
    type_info: ?TypeInfo,
    span: Span,
    visibility: Visibility,
    documentation: ?[]const u8,
    parent: ?*Symbol,
    children: []Symbol,

    pub const Kind = enum {
        function,
        variable,
        parameter,
        field,
        struct_type,
        enum_type,
        enum_variant,
        trait_type,
        type_alias,
        constant,
        module,
    };

    pub const Visibility = enum {
        private,
        public,
        package,
    };
};
```

### TypeInfo

```zig
pub const TypeInfo = struct {
    display: []const u8,     // Human-readable: "fn(i32) -> string"
    kind: Kind,

    pub const Kind = enum {
        primitive,
        struct_,
        enum_,
        trait_,
        function,
        reference,
        option,
        result,
        array,
        slice,
        tuple,
        generic,
        unknown,
    };
};
```

### CompletionItem

```zig
pub const CompletionItem = struct {
    label: []const u8,
    kind: Kind,
    type_info: ?[]const u8,
    documentation: ?[]const u8,
    insert_text: ?[]const u8,
    sort_priority: u8,

    pub const Kind = enum {
        function,
        method,
        field,
        variable,
        type,
        keyword,
        snippet,
    };
};
```

---

## Implementation Plan

### Phase 1: Foundation

**Goal:** Basic diagnostics working

1. **MCP Protocol Layer**
   - JSON-RPC parser/serializer
   - stdio transport handler
   - Tool dispatch router

2. **Compiler Integration**
   - Wire up existing lexer
   - Complete parser (expressions, statements, declarations)
   - Basic type checker (enough to catch obvious errors)

3. **klar_check Tool**
   - Parse file, run type checker
   - Collect and return diagnostics
   - Handle content override for unsaved files

**Deliverable:** Claude can validate Klar code snippets

### Phase 2: Type Information

**Goal:** Hover and go-to-definition working

1. **Symbol Table**
   - Build during type checking
   - Map positions to symbols
   - Track definition locations

2. **klar_hover Tool**
   - Look up symbol at position
   - Return type and documentation

3. **klar_goto_definition Tool**
   - Find symbol at position
   - Return definition location

**Deliverable:** Claude can explore existing Klar code

### Phase 3: Navigation

**Goal:** Full code navigation

1. **Reference Index**
   - Track all usages during analysis
   - Index by symbol ID

2. **klar_references Tool**
   - Find all usages of symbol

3. **klar_symbols Tool**
   - List file/project symbols
   - Support filtering

**Deliverable:** Claude can understand code structure

### Phase 4: Completions

**Goal:** Context-aware suggestions

1. **Completion Engine**
   - Analyze context (after `.`, in type position, etc.)
   - Filter by accessibility and relevance
   - Sort by likelihood

2. **klar_completions Tool**
   - Generate suggestions at position
   - Include type info and docs

**Deliverable:** Claude can suggest valid code

---

## Project Structure

```
klar/
├── build.zig
├── build.zig.zon
├── src/
│   ├── main.zig           # CLI entry point
│   ├── mcp/
│   │   ├── server.zig     # MCP server main loop
│   │   ├── protocol.zig   # JSON-RPC types
│   │   ├── transport.zig  # stdio handling
│   │   ├── router.zig     # Tool dispatch
│   │   └── tools/
│   │       ├── check.zig
│   │       ├── hover.zig
│   │       ├── definition.zig
│   │       ├── references.zig
│   │       ├── completions.zig
│   │       └── symbols.zig
│   ├── compiler/
│   │   ├── lexer.zig
│   │   ├── token.zig
│   │   ├── ast.zig
│   │   ├── parser.zig
│   │   ├── types.zig
│   │   └── checker.zig
│   ├── analysis/
│   │   ├── symbols.zig    # Symbol table
│   │   ├── references.zig # Reference index
│   │   └── completion.zig # Completion engine
│   └── utils/
│       ├── json.zig       # JSON serialization
│       ├── position.zig   # Position utilities
│       └── diagnostic.zig # Diagnostic types
├── std/
│   └── *.kl               # Klar standard library
└── test/
    ├── mcp_test.zig
    ├── parser_test.zig
    └── fixtures/
        └── *.kl
```

---

## Configuration

### Claude Code Settings

Add to `~/.claude/settings.json`:

```json
{
  "mcpServers": {
    "klar": {
      "command": "mcp-klar",
      "args": [],
      "env": {
        "KLAR_STD_PATH": "/path/to/klar/std"
      }
    }
  }
}
```

### Project Configuration

Optional `klar.json` in project root:

```json
{
  "std_path": "./std",
  "include_paths": ["./vendor"],
  "target": "native",
  "features": {
    "async": true,
    "unsafe": "warn"
  }
}
```

---

## Error Handling

### Graceful Degradation

The server should never crash. On errors:

1. **Parse errors** — Return partial diagnostics
2. **Type errors** — Return what's known, mark unknowns
3. **File not found** — Return clear error message
4. **Invalid position** — Return empty result

### Error Response Format

```json
{
  "error": {
    "code": -32602,
    "message": "File not found: src/missing.kl"
  }
}
```

---

## Testing Strategy

### Unit Tests

- Lexer token output
- Parser AST structure
- Type checker diagnostics
- Symbol table construction

### Integration Tests

- MCP protocol round-trips
- Tool input/output validation
- Multi-file project scenarios

### Snapshot Tests

- Known Klar files → expected diagnostics
- Known positions → expected hover info
- Regression prevention

---

## Future Work

### Phase 5: Advanced Features

- **Incremental parsing** — Only re-parse changed regions
- **Background indexing** — Index project on startup
- **Caching** — Persist analysis between sessions
- **Workspace support** — Multi-root projects

### Phase 6: Klar Self-Hosting

Once Klar can run real programs:

1. Implement MCP protocol in Klar's std
2. Port tool handlers to Klar
3. Use Klar MCP server to develop Klar

This creates a virtuous cycle where improving the language improves the tooling.

### Potential Additional Tools

- `klar_format` — Format code
- `klar_rename` — Rename symbol across project
- `klar_fix` — Auto-fix certain diagnostics
- `klar_explain` — Detailed error explanations
- `klar_ast` — Dump AST for debugging

---

## References

- [Model Context Protocol Specification](https://modelcontextprotocol.io/specification)
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
- [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) (for inspiration)
- [Klar Language Specification](./klar-language-spec.md)

---

*Document version: 1.0*
*Last updated: January 2025*
