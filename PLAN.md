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
- [ ] Write MCP protocol integration tests

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

## Phase 8: Symbol Table & Analysis

- [ ] Implement `src/analysis/symbols.zig` (Symbol, TypeInfo structs)
- [ ] Build symbol table during type checking
- [ ] Map positions to symbols
- [ ] Track definition locations
- [ ] Support nested scopes
- [ ] Write symbol table tests

## Phase 9: klar_hover Tool

- [ ] Implement `src/mcp/tools/hover.zig`
- [ ] Look up symbol at position
- [ ] Return type, kind, documentation, definition location
- [ ] Handle edge cases (invalid position, no symbol)
- [ ] Write hover tool tests

## Phase 10: klar_goto_definition Tool

- [ ] Implement `src/mcp/tools/definition.zig`
- [ ] Find symbol at position
- [ ] Return definition file, line, column, kind, preview
- [ ] Handle multi-definition cases (e.g., trait methods)
- [ ] Write go-to-definition tests

**Milestone: Phase 2 Complete - Claude can explore existing Klar code**

## Phase 11: Reference Index

- [ ] Implement `src/analysis/references.zig`
- [ ] Track all usages during analysis
- [ ] Index by symbol ID
- [ ] Support incremental updates
- [ ] Write reference index tests

## Phase 12: klar_references Tool

- [ ] Implement `src/mcp/tools/references.zig`
- [ ] Find all usages of symbol at position
- [ ] Return file, line, column, context for each reference
- [ ] Support `include_definition` parameter
- [ ] Write references tool tests

## Phase 13: klar_symbols Tool

- [ ] Implement `src/mcp/tools/symbols.zig`
- [ ] List all symbols in a file
- [ ] Support `kind` filter parameter
- [ ] Include nested children (fields, variants)
- [ ] Return visibility and type info
- [ ] Write symbols tool tests

**Milestone: Phase 3 Complete - Claude can understand code structure**

## Phase 14: Completion Engine

- [ ] Implement `src/analysis/completion.zig`
- [ ] Analyze completion context (after `.`, in type position, etc.)
- [ ] Filter completions by accessibility
- [ ] Sort by relevance/likelihood
- [ ] Support method completions on receiver types
- [ ] Support field completions for structs
- [ ] Support keyword completions
- [ ] Write completion engine tests

## Phase 15: klar_completions Tool

- [ ] Implement `src/mcp/tools/completions.zig`
- [ ] Generate suggestions at position
- [ ] Include type info, documentation, insert_text
- [ ] Return context info (receiver_type, trigger)
- [ ] Write completions tool tests

**Milestone: Phase 4 Complete - Claude can suggest valid code**

## Phase 16: Document Cache

- [ ] Implement document cache in server
- [ ] Store file contents and parsed ASTs
- [ ] Invalidate on file changes
- [ ] Cache analysis results
- [ ] Measure and optimize performance

## Phase 17: MCP Resources

- [ ] Implement `klar://std/docs` resource for standard library docs
- [ ] Implement `klar://project/structure` resource for project tree
- [ ] Register resources in MCP initialization
- [ ] Write resource tests

## Phase 18: Configuration & Polish

- [ ] Implement `klar.json` project configuration parsing
- [ ] Support `KLAR_STD_PATH` environment variable
- [ ] Implement graceful error handling (never crash)
- [ ] Add comprehensive error messages
- [ ] Write end-to-end integration tests
- [ ] Document installation in README

## Phase 19: Standard Library

- [ ] Create `std/` directory structure
- [ ] Implement core types (String, List, Option, Result)
- [ ] Implement basic I/O primitives
- [ ] Write standard library documentation
- [ ] Test standard library type resolution

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
