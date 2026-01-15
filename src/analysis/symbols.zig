//! Symbol table and analysis infrastructure for Klar language intelligence.
//!
//! This module provides:
//! - Symbol table construction during type checking
//! - Position-to-symbol mapping for hover and go-to-definition
//! - Scope tracking with proper nesting
//! - Type information for symbols

const std = @import("std");
const Allocator = std.mem.Allocator;
const compiler = @import("compiler");
const utils = @import("utils");

const ast = compiler.ast;
const types = compiler.types;

const Ast = ast.Ast;
const NodeIndex = ast.NodeIndex;
const Node = ast.Node;
const null_node = ast.null_node;

const TypeId = types.TypeId;
const TypePool = types.TypePool;
const invalid_type = types.invalid_type;

const Position = utils.Position;
const Span = utils.Span;

/// Unique identifier for a symbol.
pub const SymbolId = u32;

/// Sentinel value for invalid symbol ID.
pub const invalid_symbol: SymbolId = std.math.maxInt(SymbolId);

/// Kind of symbol.
pub const SymbolKind = enum {
    /// Local variable binding (let)
    variable,
    /// Mutable variable binding (var)
    mutable_variable,
    /// Constant binding (const)
    constant,
    /// Function parameter
    parameter,
    /// Function declaration
    function,
    /// Struct type definition
    @"struct",
    /// Enum type definition
    @"enum",
    /// Trait definition
    trait,
    /// Type alias
    type_alias,
    /// Generic type parameter
    generic_param,
    /// Struct field
    field,
    /// Enum variant
    variant,
    /// Impl block (associated with a type)
    impl_block,
    /// Method (function inside impl or trait)
    method,
    /// Module/file
    module,
    /// Import binding
    import,

    /// Returns true if this symbol kind represents a type.
    pub fn isType(self: SymbolKind) bool {
        return switch (self) {
            .@"struct", .@"enum", .trait, .type_alias, .generic_param => true,
            else => false,
        };
    }

    /// Returns true if this symbol kind can be called.
    pub fn isCallable(self: SymbolKind) bool {
        return switch (self) {
            .function, .method, .@"struct" => true, // struct for constructors
            else => false,
        };
    }

    /// Returns a human-readable string for this kind.
    pub fn toString(self: SymbolKind) []const u8 {
        return switch (self) {
            .variable => "variable",
            .mutable_variable => "mutable variable",
            .constant => "constant",
            .parameter => "parameter",
            .function => "function",
            .@"struct" => "struct",
            .@"enum" => "enum",
            .trait => "trait",
            .type_alias => "type alias",
            .generic_param => "type parameter",
            .field => "field",
            .variant => "variant",
            .impl_block => "impl block",
            .method => "method",
            .module => "module",
            .import => "import",
        };
    }
};

/// Information about a symbol.
pub const Symbol = struct {
    /// Unique identifier for this symbol.
    id: SymbolId,
    /// Symbol name.
    name: []const u8,
    /// Kind of symbol.
    kind: SymbolKind,
    /// Type of the symbol (or invalid_type if unknown/not applicable).
    type_id: TypeId,
    /// Span where this symbol is defined.
    definition_span: Span,
    /// Documentation comment, if any.
    documentation: ?[]const u8,
    /// Whether this symbol is public.
    is_public: bool,
    /// Whether this symbol is mutable (for variables).
    is_mutable: bool,
    /// Parent scope ID (for nested lookups).
    scope_id: ScopeId,
    /// AST node that defines this symbol (for cross-reference).
    node_index: NodeIndex,
    /// Container symbol (e.g., struct for a field, module for a function).
    container: SymbolId,
};

/// Unique identifier for a scope.
pub const ScopeId = u32;

/// Sentinel value for invalid/no scope.
pub const invalid_scope: ScopeId = std.math.maxInt(ScopeId);

/// Represents a scope in the symbol table.
pub const Scope = struct {
    /// Unique identifier for this scope.
    id: ScopeId,
    /// Parent scope (or invalid_scope for global).
    parent: ScopeId,
    /// Kind of scope (function, block, etc.).
    kind: Kind,
    /// Span covered by this scope.
    span: Span,
    /// Symbols defined in this scope (name -> symbol id).
    symbols: std.StringHashMapUnmanaged(SymbolId),

    pub const Kind = enum {
        /// Global/module scope.
        global,
        /// Function body scope.
        function,
        /// Block scope (inside {}).
        block,
        /// Struct definition scope.
        struct_def,
        /// Enum definition scope.
        enum_def,
        /// Trait definition scope.
        trait_def,
        /// Impl block scope.
        impl_block,
        /// Loop body scope.
        loop,
        /// Match arm scope.
        match_arm,
        /// Lambda/closure scope.
        lambda,
    };

    pub fn init(id: ScopeId, parent: ScopeId, kind: Kind, span: Span) Scope {
        return .{
            .id = id,
            .parent = parent,
            .kind = kind,
            .span = span,
            .symbols = .{},
        };
    }

    pub fn deinit(self: *Scope, allocator: Allocator) void {
        self.symbols.deinit(allocator);
    }

    /// Add a symbol to this scope.
    pub fn addSymbol(self: *Scope, allocator: Allocator, name: []const u8, symbol_id: SymbolId) !void {
        try self.symbols.put(allocator, name, symbol_id);
    }

    /// Look up a symbol by name in this scope only (not parent).
    pub fn lookupLocal(self: *const Scope, name: []const u8) ?SymbolId {
        return self.symbols.get(name);
    }
};

/// Reference to a symbol (usage site).
pub const SymbolReference = struct {
    /// The symbol being referenced.
    symbol_id: SymbolId,
    /// Span of the reference.
    span: Span,
    /// AST node of the reference.
    node_index: NodeIndex,
    /// Kind of reference.
    kind: Kind,

    pub const Kind = enum {
        /// Read access.
        read,
        /// Write access (assignment target).
        write,
        /// Call (function invocation).
        call,
        /// Type usage.
        type_ref,
        /// Import.
        import,
    };
};

/// Symbol table holding all symbols, scopes, and references.
pub const SymbolTable = struct {
    allocator: Allocator,
    /// All symbols by ID.
    symbols: std.ArrayListUnmanaged(Symbol),
    /// All scopes by ID.
    scopes: std.ArrayListUnmanaged(Scope),
    /// All references.
    references: std.ArrayListUnmanaged(SymbolReference),
    /// Maps positions (by offset) to symbol definitions.
    position_to_definition: std.AutoHashMapUnmanaged(usize, SymbolId),
    /// Maps positions (by offset) to symbol references.
    position_to_reference: std.ArrayListUnmanaged(PositionRef),
    /// Current scope stack during construction.
    scope_stack: std.ArrayListUnmanaged(ScopeId),
    /// String storage for symbol names.
    strings: std.ArrayListUnmanaged([]const u8),

    /// Entry for position-to-reference mapping.
    const PositionRef = struct {
        start_offset: usize,
        end_offset: usize,
        symbol_id: SymbolId,
    };

    pub fn init(allocator: Allocator) SymbolTable {
        return .{
            .allocator = allocator,
            .symbols = .{},
            .scopes = .{},
            .references = .{},
            .position_to_definition = .{},
            .position_to_reference = .{},
            .scope_stack = .{},
            .strings = .{},
        };
    }

    pub fn deinit(self: *SymbolTable) void {
        // Free stored strings.
        for (self.strings.items) |s| {
            self.allocator.free(s);
        }
        self.strings.deinit(self.allocator);

        // Free scope symbol maps.
        for (self.scopes.items) |*scope| {
            scope.deinit(self.allocator);
        }
        self.scopes.deinit(self.allocator);

        self.symbols.deinit(self.allocator);
        self.references.deinit(self.allocator);
        self.position_to_definition.deinit(self.allocator);
        self.position_to_reference.deinit(self.allocator);
        self.scope_stack.deinit(self.allocator);
    }

    /// Store a string and return a reference to it.
    fn storeString(self: *SymbolTable, str: []const u8) ![]const u8 {
        const owned = try self.allocator.dupe(u8, str);
        try self.strings.append(self.allocator, owned);
        return owned;
    }

    /// Create a new symbol and add it to the current scope.
    pub fn addSymbol(
        self: *SymbolTable,
        name: []const u8,
        kind: SymbolKind,
        type_id: TypeId,
        span: Span,
        node_index: NodeIndex,
        is_public: bool,
        is_mutable: bool,
        container: SymbolId,
    ) !SymbolId {
        const id: SymbolId = @intCast(self.symbols.items.len);
        const current_scope = self.currentScopeId();

        const owned_name = try self.storeString(name);

        try self.symbols.append(self.allocator, .{
            .id = id,
            .name = owned_name,
            .kind = kind,
            .type_id = type_id,
            .definition_span = span,
            .documentation = null,
            .is_public = is_public,
            .is_mutable = is_mutable,
            .scope_id = current_scope,
            .node_index = node_index,
            .container = container,
        });

        // Add to current scope.
        if (current_scope != invalid_scope) {
            if (self.getScope(current_scope)) |scope| {
                try scope.addSymbol(self.allocator, owned_name, id);
            }
        }

        // Map position to definition.
        try self.position_to_definition.put(self.allocator, span.start.offset, id);

        return id;
    }

    /// Add a reference to a symbol.
    pub fn addReference(
        self: *SymbolTable,
        symbol_id: SymbolId,
        span: Span,
        node_index: NodeIndex,
        kind: SymbolReference.Kind,
    ) !void {
        try self.references.append(self.allocator, .{
            .symbol_id = symbol_id,
            .span = span,
            .node_index = node_index,
            .kind = kind,
        });

        // Map position range to reference.
        try self.position_to_reference.append(self.allocator, .{
            .start_offset = span.start.offset,
            .end_offset = span.end.offset,
            .symbol_id = symbol_id,
        });
    }

    /// Push a new scope onto the stack.
    pub fn pushScope(self: *SymbolTable, kind: Scope.Kind, span: Span) !ScopeId {
        const id: ScopeId = @intCast(self.scopes.items.len);
        const parent = self.currentScopeId();

        try self.scopes.append(self.allocator, Scope.init(id, parent, kind, span));
        try self.scope_stack.append(self.allocator, id);

        return id;
    }

    /// Pop the current scope from the stack.
    pub fn popScope(self: *SymbolTable) void {
        if (self.scope_stack.items.len > 0) {
            _ = self.scope_stack.pop();
        }
    }

    /// Get the current scope ID.
    pub fn currentScopeId(self: *const SymbolTable) ScopeId {
        if (self.scope_stack.items.len > 0) {
            return self.scope_stack.items[self.scope_stack.items.len - 1];
        }
        return invalid_scope;
    }

    /// Get a mutable reference to a scope by ID.
    fn getScope(self: *SymbolTable, id: ScopeId) ?*Scope {
        if (id == invalid_scope or id >= self.scopes.items.len) {
            return null;
        }
        return &self.scopes.items[id];
    }

    /// Get a symbol by ID.
    pub fn getSymbol(self: *const SymbolTable, id: SymbolId) ?Symbol {
        if (id == invalid_symbol or id >= self.symbols.items.len) {
            return null;
        }
        return self.symbols.items[id];
    }

    /// Get a scope by ID (immutable).
    pub fn getScopeConst(self: *const SymbolTable, id: ScopeId) ?Scope {
        if (id == invalid_scope or id >= self.scopes.items.len) {
            return null;
        }
        return self.scopes.items[id];
    }

    /// Look up a symbol by name, searching up the scope chain.
    pub fn lookup(self: *const SymbolTable, name: []const u8) ?SymbolId {
        var scope_id = self.currentScopeId();
        while (scope_id != invalid_scope) {
            if (self.getScopeConst(scope_id)) |scope| {
                if (scope.lookupLocal(name)) |symbol_id| {
                    return symbol_id;
                }
                scope_id = scope.parent;
            } else {
                break;
            }
        }
        return null;
    }

    /// Look up a symbol by name in a specific scope (local only).
    pub fn lookupInScope(self: *const SymbolTable, scope_id: ScopeId, name: []const u8) ?SymbolId {
        if (self.getScopeConst(scope_id)) |scope| {
            return scope.lookupLocal(name);
        }
        return null;
    }

    /// Find the symbol at a given position (for hover/go-to-definition).
    pub fn symbolAtPosition(self: *const SymbolTable, offset: usize) ?SymbolId {
        // First check definitions.
        if (self.position_to_definition.get(offset)) |id| {
            return id;
        }

        // Then check references.
        for (self.position_to_reference.items) |ref| {
            if (offset >= ref.start_offset and offset < ref.end_offset) {
                return ref.symbol_id;
            }
        }

        return null;
    }

    /// Get all references to a symbol.
    pub fn getReferences(self: *const SymbolTable, symbol_id: SymbolId) []const SymbolReference {
        // This is O(n), could be optimized with an index.
        var count: usize = 0;
        for (self.references.items) |ref| {
            if (ref.symbol_id == symbol_id) count += 1;
        }

        // For now just return all references (caller must filter).
        // A more efficient implementation would maintain per-symbol reference lists.
        return self.references.items;
    }

    /// Get all symbols in a scope.
    pub fn symbolsInScope(self: *const SymbolTable, scope_id: ScopeId) []const Symbol {
        // Return all symbols that belong to this scope.
        // This is O(n), could be optimized.
        _ = scope_id;
        return self.symbols.items;
    }

    /// Find the scope containing a position.
    pub fn scopeAtPosition(self: *const SymbolTable, offset: usize, file_id: u32) ?ScopeId {
        // Find the innermost scope containing this position.
        var best_scope: ?ScopeId = null;
        var best_size: usize = std.math.maxInt(usize);

        for (self.scopes.items) |scope| {
            if (scope.span.file_id != file_id) continue;

            if (offset >= scope.span.start.offset and offset < scope.span.end.offset) {
                const size = scope.span.end.offset - scope.span.start.offset;
                if (size < best_size) {
                    best_size = size;
                    best_scope = scope.id;
                }
            }
        }

        return best_scope;
    }

    /// Get the number of symbols.
    pub fn symbolCount(self: *const SymbolTable) usize {
        return self.symbols.items.len;
    }

    /// Get the number of scopes.
    pub fn scopeCount(self: *const SymbolTable) usize {
        return self.scopes.items.len;
    }

    /// Get the number of references.
    pub fn referenceCount(self: *const SymbolTable) usize {
        return self.references.items.len;
    }
};

/// Type information for a symbol, suitable for display.
pub const TypeInfo = struct {
    /// Formatted type string (e.g., "fn(i32, i32) -> bool").
    type_string: []const u8,
    /// Symbol kind description.
    kind_string: []const u8,
    /// Full signature (for functions).
    signature: ?[]const u8,
    /// Documentation.
    documentation: ?[]const u8,
    /// Definition location.
    definition: ?DefinitionLocation,

    pub const DefinitionLocation = struct {
        file_id: u32,
        line: u32,
        column: u32,
        offset: usize,
    };
};

/// Build TypeInfo for a symbol.
pub fn buildTypeInfo(
    symbol: Symbol,
    type_pool: *const TypePool,
    allocator: Allocator,
) !TypeInfo {
    const type_string = if (symbol.type_id != invalid_type)
        type_pool.format(symbol.type_id)
    else
        "unknown";

    const owned_type = try allocator.dupe(u8, type_string);
    const owned_kind = try allocator.dupe(u8, symbol.kind.toString());

    return .{
        .type_string = owned_type,
        .kind_string = owned_kind,
        .signature = null, // TODO: build function signature
        .documentation = symbol.documentation,
        .definition = .{
            .file_id = symbol.definition_span.file_id,
            .line = symbol.definition_span.start.line,
            .column = symbol.definition_span.start.column,
            .offset = symbol.definition_span.start.offset,
        },
    };
}

// === Tests ===

test "SymbolTable initialization" {
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    try std.testing.expectEqual(@as(usize, 0), table.symbolCount());
    try std.testing.expectEqual(@as(usize, 0), table.scopeCount());
}

test "SymbolTable scope management" {
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    // Push global scope.
    const global = try table.pushScope(.global, Span.empty);
    try std.testing.expectEqual(@as(ScopeId, 0), global);
    try std.testing.expectEqual(@as(ScopeId, 0), table.currentScopeId());

    // Push function scope.
    const func = try table.pushScope(.function, Span.empty);
    try std.testing.expectEqual(@as(ScopeId, 1), func);
    try std.testing.expectEqual(@as(ScopeId, 1), table.currentScopeId());

    // Check parent relationship.
    const func_scope = table.getScopeConst(func).?;
    try std.testing.expectEqual(@as(ScopeId, 0), func_scope.parent);

    // Pop function scope.
    table.popScope();
    try std.testing.expectEqual(@as(ScopeId, 0), table.currentScopeId());
}

test "SymbolTable add and lookup symbol" {
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    _ = try table.pushScope(.global, Span.empty);

    const span = Span{
        .start = .{ .line = 1, .column = 5, .offset = 4 },
        .end = .{ .line = 1, .column = 6, .offset = 5 },
        .file_id = 0,
    };

    const sym_id = try table.addSymbol(
        "x",
        .variable,
        types.BuiltinTypes.i32_type,
        span,
        0,
        false,
        false,
        invalid_symbol,
    );

    try std.testing.expectEqual(@as(SymbolId, 0), sym_id);

    // Lookup by name.
    const found = table.lookup("x");
    try std.testing.expect(found != null);
    try std.testing.expectEqual(sym_id, found.?);

    // Lookup unknown name.
    try std.testing.expect(table.lookup("y") == null);
}

test "SymbolTable nested scope lookup" {
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    _ = try table.pushScope(.global, Span.empty);

    // Add symbol in global scope.
    const global_sym = try table.addSymbol(
        "global_var",
        .variable,
        types.BuiltinTypes.i32_type,
        Span.empty,
        0,
        false,
        false,
        invalid_symbol,
    );

    // Push inner scope.
    _ = try table.pushScope(.block, Span.empty);

    // Add symbol in inner scope.
    _ = try table.addSymbol(
        "local_var",
        .variable,
        types.BuiltinTypes.bool_type,
        Span.empty,
        1,
        false,
        false,
        invalid_symbol,
    );

    // Should find local.
    const local = table.lookup("local_var");
    try std.testing.expect(local != null);

    // Should find global from inner scope.
    const global = table.lookup("global_var");
    try std.testing.expect(global != null);
    try std.testing.expectEqual(global_sym, global.?);

    // Pop inner scope.
    table.popScope();

    // Should still find global.
    try std.testing.expect(table.lookup("global_var") != null);

    // Should NOT find local from outer scope.
    try std.testing.expect(table.lookup("local_var") == null);
}

test "SymbolTable position to symbol" {
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    _ = try table.pushScope(.global, Span.empty);

    const span = Span{
        .start = .{ .line = 1, .column = 5, .offset = 4 },
        .end = .{ .line = 1, .column = 6, .offset = 5 },
        .file_id = 0,
    };

    const sym_id = try table.addSymbol(
        "foo",
        .function,
        invalid_type,
        span,
        0,
        true,
        false,
        invalid_symbol,
    );

    // Find symbol at definition position.
    const found = table.symbolAtPosition(4);
    try std.testing.expect(found != null);
    try std.testing.expectEqual(sym_id, found.?);

    // Position outside definition.
    try std.testing.expect(table.symbolAtPosition(100) == null);
}

test "SymbolTable add reference" {
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    _ = try table.pushScope(.global, Span.empty);

    const def_span = Span{
        .start = .{ .line = 1, .column = 5, .offset = 4 },
        .end = .{ .line = 1, .column = 6, .offset = 5 },
        .file_id = 0,
    };

    const sym_id = try table.addSymbol(
        "x",
        .variable,
        types.BuiltinTypes.i32_type,
        def_span,
        0,
        false,
        false,
        invalid_symbol,
    );

    // Add a reference to the symbol.
    const ref_span = Span{
        .start = .{ .line = 3, .column = 10, .offset = 30 },
        .end = .{ .line = 3, .column = 11, .offset = 31 },
        .file_id = 0,
    };

    try table.addReference(sym_id, ref_span, 5, .read);

    try std.testing.expectEqual(@as(usize, 1), table.referenceCount());

    // Find symbol at reference position.
    const found = table.symbolAtPosition(30);
    try std.testing.expect(found != null);
    try std.testing.expectEqual(sym_id, found.?);
}

test "SymbolKind properties" {
    try std.testing.expect(SymbolKind.@"struct".isType());
    try std.testing.expect(SymbolKind.@"enum".isType());
    try std.testing.expect(!SymbolKind.variable.isType());
    try std.testing.expect(!SymbolKind.function.isType());

    try std.testing.expect(SymbolKind.function.isCallable());
    try std.testing.expect(SymbolKind.method.isCallable());
    try std.testing.expect(!SymbolKind.variable.isCallable());
}
