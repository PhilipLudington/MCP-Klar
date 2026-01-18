# Klar MCP Server Implementation Plan

> Zig-based MCP server providing Klar language intelligence to Claude Code

## Phase 1: Project Setup ✅

- [x] Initialize Zig project with `build.zig` and `build.zig.zon`
- [x] Create directory structure (`src/mcp/`, `src/compiler/`, `src/analysis/`, `src/utils/`, `test/`)
- [x] Set up test infrastructure with `zig build test`
- [x] Create `.gitstat.json` with test and build configuration
- [x] Create wrapper scripts (`build.sh`, `run-tests.sh`)

## Phase 2: Core Utilities ✅

- [x] Implement `src/utils/position.zig` (Position, Span structs)
- [x] Implement `src/utils/diagnostic.zig` (Diagnostic, Severity, Hint types)
- [x] Implement `src/utils/json.zig` (JSON serialization/deserialization)
- [x] Write unit tests for utilities

## Phase 3: MCP Protocol Layer ✅

- [x] Implement `src/mcp/protocol.zig` (JSON-RPC 2.0 request/response types)
- [x] Implement `src/mcp/transport.zig` (stdio read/write handling)
- [x] Implement `src/mcp/router.zig` (tool dispatch and parameter validation)
- [x] Implement `src/mcp/server.zig` (main loop, lifecycle, initialization)
- [x] Write MCP protocol integration tests

## Phase 4: Compiler Frontend - Lexer ✅

- [x] Define `src/compiler/token.zig` (token types for Klar language)
- [x] Implement `src/compiler/lexer.zig` (tokenization with position tracking)
- [x] Handle string literals, numbers, identifiers, keywords, operators
- [x] Write comprehensive lexer tests

## Phase 5: Compiler Frontend - Parser ✅

- [x] Define `src/compiler/ast.zig` (AST node types)
- [x] Implement `src/compiler/parser.zig` - expression parsing
- [x] Implement statement parsing (let, return, if, while, for)
- [x] Implement declaration parsing (fn, struct, enum, trait, type alias)
- [x] Implement module/import parsing
- [x] Write parser tests with fixture files

## Phase 6: Compiler Frontend - Type Checker ✅

- [x] Define `src/compiler/types.zig` (type representation)
- [x] Implement `src/compiler/checker.zig` - basic type checking
- [x] Implement type inference for variables
- [x] Implement function signature checking
- [x] Implement struct/enum type checking
- [x] Implement generic type handling
- [x] Collect diagnostics during type checking
- [x] Write type checker tests

## Phase 7: klar_check Tool (MVP) ✅

- [x] Implement `src/mcp/tools/check.zig`
- [x] Parse file and run type checker
- [x] Format diagnostics as JSON response
- [x] Handle `content` parameter for unsaved buffers
- [x] Write integration tests for klar_check
- [ ] Test end-to-end with Claude Code

**Milestone: MVP - Claude can validate Klar code snippets**

## Phase 8: Symbol Table & Analysis ✅

- [x] Implement `src/analysis/symbols.zig` (Symbol, TypeInfo structs)
- [x] Build symbol table during type checking
- [x] Map positions to symbols
- [x] Track definition locations
- [x] Support nested scopes
- [x] Write symbol table tests

## Phase 9: klar_hover Tool ✅

- [x] Implement `src/mcp/tools/hover.zig`
- [x] Look up symbol at position
- [x] Return type, kind, documentation, definition location
- [x] Handle edge cases (invalid position, no symbol)
- [x] Write hover tool tests

## Phase 10: klar_goto_definition Tool ✅

- [x] Implement `src/mcp/tools/definition.zig`
- [x] Find symbol at position
- [x] Return definition file, line, column, kind, preview
- [x] Handle multi-definition cases (e.g., trait methods)
- [x] Write go-to-definition tests

**Milestone: Phase 2 Complete - Claude can explore existing Klar code**

## Phase 11: Reference Index ✅

- [x] Implement `src/analysis/references.zig`
- [x] Track all usages during analysis
- [x] Index by symbol ID
- [x] Support incremental updates
- [x] Write reference index tests

## Phase 12: klar_references Tool ✅

- [x] Implement `src/mcp/tools/references.zig`
- [x] Find all usages of symbol at position
- [x] Return file, line, column, context for each reference
- [x] Support `include_definition` parameter
- [x] Write references tool tests

## Phase 13: klar_symbols Tool ✅

- [x] Implement `src/mcp/tools/symbols.zig`
- [x] List all symbols in a file
- [x] Support `kind` filter parameter
- [x] Include nested children (fields, variants)
- [x] Return visibility and type info
- [x] Write symbols tool tests

**Milestone: Phase 3 Complete - Claude can understand code structure**

## Phase 14: Completion Engine ✅

- [x] Implement `src/analysis/completion.zig`
- [x] Analyze completion context (after `.`, in type position, etc.)
- [x] Filter completions by accessibility
- [x] Sort by relevance/likelihood
- [x] Support method completions on receiver types
- [x] Support field completions for structs
- [x] Support keyword completions
- [x] Write completion engine tests

## Phase 15: klar_completions Tool ✅

- [x] Implement `src/mcp/tools/completions.zig`
- [x] Generate suggestions at position
- [x] Include type info, documentation, insert_text
- [x] Return context info (receiver_type, trigger)
- [x] Write completions tool tests

**Milestone: Phase 4 Complete - Claude can suggest valid code**

## Phase 16: Document Cache ✅

- [x] Implement document cache in server (`src/analysis/cache.zig`)
- [x] Store file contents and parsed ASTs (CacheEntry with content hash)
- [x] Invalidate on file changes (hash-based invalidation)
- [x] Cache analysis results (AST and Checker stored per entry)
- [x] Measure and optimize performance (CacheStats with hit rate tracking)

**Implementation notes:**
- `DocumentCache` stores parsed ASTs and type-checked results keyed by content hash
- LRU eviction when cache reaches capacity (configurable max_entries)
- `cached_analysis.zig` helper module for tools to use cached analysis
- `klar_check` tool refactored to use the helper module
- Statistics tracking: hits, misses, evictions, hit rate

**Milestone: Performance Optimization - Redundant parsing/analysis eliminated**

## Phase 17: MCP Resources ✅

- [x] Implement `klar://std/docs` resource for standard library docs
- [x] Implement `klar://project/structure` resource for project tree
- [x] Register resources in MCP initialization
- [x] Write resource tests

**Implementation notes:**
- Added `ResourceHandler` type and `registerResource()` method to Router
- Added `resources/list` and `resources/read` dispatch handlers
- `klar://std/docs` returns comprehensive Markdown documentation for Klar language
- `klar://project/structure` scans current directory for `.kl` files and builds a tree
- Resources capability advertised during MCP initialization

## Phase 18: Configuration & Polish ✅

- [x] Implement `klar.json` project configuration parsing
- [x] Support `KLAR_STD_PATH` environment variable
- [x] Implement graceful error handling (never crash)
- [x] Add comprehensive error messages
- [x] Write end-to-end integration tests
- [x] Document installation in README

**Implementation notes:**
- Created `src/config.zig` with `RuntimeConfig` struct for runtime configuration
- Environment variable support: `KLAR_STD_PATH` and `KLAR_VERBOSE`
- `klar.json` parsing with std_path, project_root, source_dirs, verbose options
- Configuration precedence: env vars > klar.json > defaults
- Server main loop catches errors and continues serving (never crashes)
- Improved error messages include tool/resource names and error context
- Integration tests in `test/integration_test.zig` covering MCP lifecycle, error handling, config

## Phase 19: Standard Library ✅

- [x] Create `std/` directory structure
- [x] Implement core types (String, List, Option, Result)
- [x] Implement basic I/O primitives
- [x] Write standard library documentation
- [x] Test standard library type resolution

**Implementation notes:**
- Created `std/` directory with modular structure
- Core types implemented: `Option<T>`, `Result<T, E>`, `String`, `List<T>`, `Map<K, V>`, `Set<T>`
- Traits defined: `Display`, `Debug`, `Clone`, `Eq`, `Ord`, `Hash`, `Default`, `Iterator`
- I/O primitives: `Reader`, `Writer`, `BufReader`, `BufWriter`, `File`, `stdin/stdout/stderr`
- Documentation in `std/README.md` with usage examples
- Integration tests added for parsing generic types, traits, and type checking

## Future Work (Not in Scope for Phase 1)

Tracked for reference:
- Incremental parsing
- Background indexing
- Analysis caching between sessions
- Workspace/multi-root project support
- `klar_format` tool
- `klar_rename` tool
- `klar_fix` auto-fix tool
- `klar_explain` detailed error explanations
- Self-hosting in Klar
