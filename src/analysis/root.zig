//! Analysis layer for Klar MCP Server.
//!
//! Provides symbol tables, reference indexing, and completion engine.

pub const symbols = @import("symbols.zig");
pub const builder = @import("builder.zig");
pub const references = @import("references.zig");
pub const completion = @import("completion.zig");

// Re-export main types.
pub const SymbolTable = symbols.SymbolTable;
pub const Symbol = symbols.Symbol;
pub const SymbolId = symbols.SymbolId;
pub const SymbolKind = symbols.SymbolKind;
pub const Scope = symbols.Scope;
pub const ScopeId = symbols.ScopeId;
pub const SymbolReference = symbols.SymbolReference;
pub const TypeInfo = symbols.TypeInfo;

pub const SymbolTableBuilder = builder.SymbolTableBuilder;
pub const buildSymbolTable = builder.buildSymbolTable;

pub const invalid_symbol = symbols.invalid_symbol;
pub const invalid_scope = symbols.invalid_scope;

pub const ReferenceIndex = references.ReferenceIndex;
pub const ReferenceResult = references.ReferenceResult;
pub const findReferences = references.findReferences;
pub const freeReferenceResult = references.freeReferenceResult;

pub const CompletionEngine = completion.CompletionEngine;
pub const CompletionItem = completion.CompletionItem;
pub const CompletionItemKind = completion.CompletionItemKind;
pub const CompletionContext = completion.CompletionContext;
pub const CompletionResult = completion.CompletionResult;

test {
    _ = symbols;
    _ = builder;
    _ = references;
    _ = completion;
}
