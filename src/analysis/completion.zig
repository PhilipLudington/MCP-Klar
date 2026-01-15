//! Completion engine for Klar language intelligence.
//!
//! This module provides code completion functionality by analyzing the
//! context at a cursor position and generating relevant suggestions.

const std = @import("std");
const Allocator = std.mem.Allocator;
const compiler = @import("compiler");
const utils = @import("utils");

const ast = compiler.ast;
const types = compiler.types;
const checker_mod = compiler.checker;

const Ast = ast.Ast;
const Node = ast.Node;
const NodeIndex = ast.NodeIndex;
const null_node = ast.null_node;

const TypeId = types.TypeId;
const TypePool = types.TypePool;
const BuiltinTypes = types.BuiltinTypes;
const invalid_type = types.invalid_type;

const Checker = checker_mod.Checker;
const Symbol = checker_mod.Symbol;
const Scope = checker_mod.Scope;

const Position = utils.Position;
const Span = utils.Span;

const log = std.log.scoped(.completion);

/// Kind of completion item.
pub const CompletionItemKind = enum {
    variable,
    function,
    method,
    field,
    @"struct",
    @"enum",
    trait,
    type_alias,
    keyword,
    snippet,
    constant,
    parameter,
    variant,
    module,

    /// Convert to LSP completion kind number.
    pub fn toLspKind(self: CompletionItemKind) u8 {
        return switch (self) {
            .variable => 6, // Variable
            .function => 3, // Function
            .method => 2, // Method
            .field => 5, // Field
            .@"struct" => 22, // Struct
            .@"enum" => 13, // Enum
            .trait => 8, // Interface
            .type_alias => 22, // Struct (used for type)
            .keyword => 14, // Keyword
            .snippet => 15, // Snippet
            .constant => 21, // Constant
            .parameter => 6, // Variable
            .variant => 20, // EnumMember
            .module => 9, // Module
        };
    }

    /// Convert from Symbol.Kind.
    pub fn fromSymbolKind(kind: Symbol.Kind) CompletionItemKind {
        return switch (kind) {
            .variable => .variable,
            .parameter => .parameter,
            .function => .function,
            .@"struct" => .@"struct",
            .@"enum" => .@"enum",
            .trait => .trait,
            .type_alias => .type_alias,
            .generic_param => .type_alias,
            .field => .field,
            .variant => .variant,
        };
    }
};

/// A completion suggestion.
pub const CompletionItem = struct {
    /// Display label for the completion.
    label: []const u8,
    /// Kind of completion item.
    kind: CompletionItemKind,
    /// Detailed type information.
    detail: ?[]const u8,
    /// Documentation string.
    documentation: ?[]const u8,
    /// Text to insert (may differ from label for snippets).
    insert_text: ?[]const u8,
    /// Sort text for ordering (lower = higher priority).
    sort_text: ?[]const u8,
    /// Whether this is deprecated.
    deprecated: bool,
};

/// Completion context describing where completions are being requested.
pub const CompletionContext = enum {
    /// General expression context (variables, functions, etc.).
    expression,
    /// After a dot operator (field/method access).
    dot_access,
    /// In a type annotation position.
    type_position,
    /// After a pattern keyword (in match arms).
    pattern,
    /// After `::` (enum variant access).
    enum_access,
    /// In an import statement.
    import_path,
    /// After `impl` keyword.
    impl_target,
    /// In a function parameter list.
    parameter,
    /// At the start of a statement.
    statement,
    /// Unknown context.
    unknown,
};

/// Trigger that caused completions to be requested.
pub const CompletionTrigger = enum {
    /// User explicitly requested completions.
    invoked,
    /// Triggered by typing a character.
    trigger_character,
    /// Triggered by identifier being typed.
    identifier,
};

/// Result of completion request with context information.
pub const CompletionResult = struct {
    /// The completion items.
    items: []const CompletionItem,
    /// Context that was detected.
    context: CompletionContext,
    /// Receiver type (for dot access).
    receiver_type: ?[]const u8,
    /// Whether completions are incomplete (more available).
    is_incomplete: bool,
};

/// Completion engine state.
pub const CompletionEngine = struct {
    allocator: Allocator,
    checker: *const Checker,
    ast_tree: *const Ast,
    source: []const u8,

    pub fn init(
        allocator: Allocator,
        checker_inst: *const Checker,
        tree: *const Ast,
        source: []const u8,
    ) CompletionEngine {
        return .{
            .allocator = allocator,
            .checker = checker_inst,
            .ast_tree = tree,
            .source = source,
        };
    }

    /// Get completions at the specified position.
    pub fn getCompletions(
        self: *CompletionEngine,
        line: u32,
        column: u32,
    ) !CompletionResult {
        const offset = calculateOffset(self.source, line, column);

        // Determine completion context.
        const context = self.analyzeContext(offset);

        var items = std.ArrayListUnmanaged(CompletionItem){};
        var receiver_type: ?[]const u8 = null;

        switch (context) {
            .dot_access => {
                // Find what's before the dot and get its type.
                const dot_info = self.findDotContext(offset);
                if (dot_info.type_id != invalid_type) {
                    try self.addMemberCompletions(&items, dot_info.type_id);
                    receiver_type = self.checker.type_pool.format(dot_info.type_id);
                }
            },
            .type_position => {
                // Offer type names.
                try self.addTypeCompletions(&items);
            },
            .enum_access => {
                // Offer enum variants.
                const enum_info = self.findEnumContext(offset);
                if (enum_info.type_id != invalid_type) {
                    try self.addVariantCompletions(&items, enum_info.type_id);
                }
            },
            .expression, .statement => {
                // Offer everything in scope.
                try self.addScopeCompletions(&items, offset);
                try self.addKeywordCompletions(&items, context);
            },
            .pattern => {
                // Offer pattern-related completions.
                try self.addPatternCompletions(&items, offset);
            },
            .import_path => {
                // Offer module names.
                try self.addModuleCompletions(&items);
            },
            .parameter => {
                // Offer type names for parameter types.
                try self.addTypeCompletions(&items);
            },
            .impl_target, .unknown => {
                // Offer types for impl target.
                try self.addTypeCompletions(&items);
            },
        }

        // Sort completions by relevance.
        self.sortCompletions(items.items, offset);

        // Take ownership of items.
        const owned_items = try self.allocator.dupe(CompletionItem, items.items);

        return .{
            .items = owned_items,
            .context = context,
            .receiver_type = receiver_type,
            .is_incomplete = false,
        };
    }

    /// Analyze the context at the given offset.
    fn analyzeContext(self: *CompletionEngine, offset: usize) CompletionContext {
        // Look backwards to find context.
        if (offset == 0) return .statement;

        const before = if (offset <= self.source.len) self.source[0..offset] else self.source;

        // Find the last non-whitespace content.
        var i = before.len;
        while (i > 0) : (i -= 1) {
            const c = before[i - 1];
            if (!std.ascii.isWhitespace(c)) break;
        }

        if (i == 0) return .statement;

        // Check for dot access.
        if (before[i - 1] == '.') {
            return .dot_access;
        }

        // Check for :: (enum variant access).
        if (i >= 2 and before[i - 2] == ':' and before[i - 1] == ':') {
            return .enum_access;
        }

        // Check for type position (after : or ->).
        if (before[i - 1] == ':') {
            // Check it's not ::
            if (i < 2 or before[i - 2] != ':') {
                return .type_position;
            }
        }

        // Check for -> (return type).
        if (i >= 2 and before[i - 2] == '-' and before[i - 1] == '>') {
            return .type_position;
        }

        // Look for keywords.
        const keywords_context = self.checkKeywordContext(before);
        if (keywords_context != .unknown) {
            return keywords_context;
        }

        // Default to expression context.
        return .expression;
    }

    /// Check if we're in a specific keyword context.
    fn checkKeywordContext(self: *CompletionEngine, before: []const u8) CompletionContext {
        _ = self;

        // Find the start of the current token/word.
        var i = before.len;
        while (i > 0) : (i -= 1) {
            const c = before[i - 1];
            if (!std.ascii.isAlphanumeric(c) and c != '_') break;
        }

        // Get the preceding word.
        var word_start = i;
        while (word_start > 0) : (word_start -= 1) {
            const c = before[word_start - 1];
            if (!std.ascii.isWhitespace(c)) break;
        }

        if (word_start == 0) return .unknown;

        // Find the word before the whitespace.
        const word_end = word_start;
        while (word_start > 0) : (word_start -= 1) {
            const c = before[word_start - 1];
            if (!std.ascii.isAlphanumeric(c) and c != '_') break;
        }

        const prev_word = before[word_start..word_end];

        if (std.mem.eql(u8, prev_word, "import")) {
            return .import_path;
        }
        if (std.mem.eql(u8, prev_word, "impl")) {
            return .impl_target;
        }
        if (std.mem.eql(u8, prev_word, "match")) {
            return .pattern;
        }

        return .unknown;
    }

    /// Find context for dot access completion.
    fn findDotContext(self: *CompletionEngine, offset: usize) struct { type_id: TypeId } {
        _ = offset;

        // For now, return invalid type. In a full implementation,
        // we would find the expression before the dot and get its type.
        // This requires walking the AST to find the node at the position.

        // Iterate through scopes to find potential receiver.
        for (self.checker.scopes.items) |scope| {
            var iter = scope.symbols.iterator();
            while (iter.next()) |entry| {
                const sym = entry.value_ptr.*;
                // Just return the first struct/enum type for now.
                if (sym.kind == .@"struct" or sym.kind == .@"enum") {
                    return .{ .type_id = sym.type_id };
                }
            }
        }

        return .{ .type_id = invalid_type };
    }

    /// Find context for enum variant access.
    fn findEnumContext(self: *CompletionEngine, offset: usize) struct { type_id: TypeId } {
        _ = offset;

        // Similar to dot context, find the enum type.
        for (self.checker.scopes.items) |scope| {
            var iter = scope.symbols.iterator();
            while (iter.next()) |entry| {
                const sym = entry.value_ptr.*;
                if (sym.kind == .@"enum") {
                    return .{ .type_id = sym.type_id };
                }
            }
        }

        return .{ .type_id = invalid_type };
    }

    /// Add completions for struct/enum members including methods.
    fn addMemberCompletions(
        self: *CompletionEngine,
        items: *std.ArrayListUnmanaged(CompletionItem),
        type_id: TypeId,
    ) !void {
        const typ = self.checker.type_pool.get(type_id) orelse return;

        switch (typ.kind) {
            .@"struct" => |s| {
                // Add fields.
                for (s.fields) |field| {
                    const field_type_str = self.checker.type_pool.format(field.type_id);
                    try items.append(self.allocator, .{
                        .label = field.name,
                        .kind = .field,
                        .detail = field_type_str,
                        .documentation = null,
                        .insert_text = null,
                        .sort_text = "0", // Fields first
                        .deprecated = false,
                    });
                }
                // Add methods from impl blocks (by searching scopes for methods).
                try self.addMethodCompletions(items, type_id);
            },
            .@"enum" => |e| {
                // Add variants.
                for (e.variants) |variant| {
                    const payload_str = if (variant.payload != invalid_type)
                        self.checker.type_pool.format(variant.payload)
                    else
                        null;
                    try items.append(self.allocator, .{
                        .label = variant.name,
                        .kind = .variant,
                        .detail = payload_str,
                        .documentation = null,
                        .insert_text = null,
                        .sort_text = "0",
                        .deprecated = false,
                    });
                }
                // Add methods from impl blocks.
                try self.addMethodCompletions(items, type_id);
            },
            .trait => |t| {
                // Add trait methods.
                for (t.methods) |method| {
                    try self.addMethodItem(items, method);
                }
            },
            .tuple => |t| {
                // Add tuple field indices.
                for (t.elements, 0..) |elem_type, idx| {
                    var buf: [16]u8 = undefined;
                    const label = std.fmt.bufPrint(&buf, "{d}", .{idx}) catch continue;
                    const label_copy = try self.allocator.dupe(u8, label);
                    try items.append(self.allocator, .{
                        .label = label_copy,
                        .kind = .field,
                        .detail = self.checker.type_pool.format(elem_type),
                        .documentation = null,
                        .insert_text = null,
                        .sort_text = null,
                        .deprecated = false,
                    });
                }
            },
            else => {},
        }
    }

    /// Add a method completion item from a Method definition.
    fn addMethodItem(
        self: *CompletionEngine,
        items: *std.ArrayListUnmanaged(CompletionItem),
        method: types.Method,
    ) !void {
        // Build method signature.
        var sig_buf: [256]u8 = undefined;
        var sig_len: usize = 0;

        const prefix = "fn(";
        @memcpy(sig_buf[sig_len .. sig_len + prefix.len], prefix);
        sig_len += prefix.len;

        for (method.params, 0..) |param, i| {
            if (i > 0) {
                @memcpy(sig_buf[sig_len .. sig_len + 2], ", ");
                sig_len += 2;
            }
            const param_str = self.checker.type_pool.format(param);
            if (sig_len + param_str.len < sig_buf.len) {
                @memcpy(sig_buf[sig_len .. sig_len + param_str.len], param_str);
                sig_len += param_str.len;
            }
        }

        const arrow = ") -> ";
        @memcpy(sig_buf[sig_len .. sig_len + arrow.len], arrow);
        sig_len += arrow.len;

        const ret_str = self.checker.type_pool.format(method.return_type);
        if (sig_len + ret_str.len < sig_buf.len) {
            @memcpy(sig_buf[sig_len .. sig_len + ret_str.len], ret_str);
            sig_len += ret_str.len;
        }

        const detail_copy = try self.allocator.dupe(u8, sig_buf[0..sig_len]);

        // Create insert text with parentheses.
        var insert_buf: [128]u8 = undefined;
        const insert_slice = std.fmt.bufPrint(&insert_buf, "{s}()", .{method.name}) catch method.name;
        const insert_copy = try self.allocator.dupe(u8, insert_slice);

        try items.append(self.allocator, .{
            .label = method.name,
            .kind = .method,
            .detail = detail_copy,
            .documentation = null,
            .insert_text = insert_copy,
            .sort_text = "1", // Methods after fields
            .deprecated = false,
        });
    }

    /// Search for methods defined in impl blocks for the given type.
    fn addMethodCompletions(
        self: *CompletionEngine,
        items: *std.ArrayListUnmanaged(CompletionItem),
        _: TypeId,
    ) !void {
        // Search through scopes for function symbols that could be methods.
        // In a full implementation, we would track impl blocks and their methods.
        // For now, we look for functions that look like methods.
        for (self.checker.scopes.items) |scope| {
            var iter = scope.symbols.iterator();
            while (iter.next()) |entry| {
                const sym = entry.value_ptr.*;
                // Look for functions - they could be methods.
                if (sym.kind == .function) {
                    const fn_type = self.checker.type_pool.get(sym.type_id);
                    if (fn_type != null and fn_type.?.kind == .function) {
                        const f = fn_type.?.kind.function;
                        // Build signature.
                        var sig_buf: [256]u8 = undefined;
                        const sig = std.fmt.bufPrint(&sig_buf, "fn({d} params) -> {s}", .{
                            f.params.len,
                            self.checker.type_pool.format(f.return_type),
                        }) catch "function";
                        const detail = try self.allocator.dupe(u8, sig);

                        try items.append(self.allocator, .{
                            .label = sym.name,
                            .kind = .function,
                            .detail = detail,
                            .documentation = null,
                            .insert_text = null,
                            .sort_text = "2", // Functions after methods
                            .deprecated = false,
                        });
                    }
                }
            }
        }
    }

    /// Add completions for enum variants.
    fn addVariantCompletions(
        self: *CompletionEngine,
        items: *std.ArrayListUnmanaged(CompletionItem),
        type_id: TypeId,
    ) !void {
        const typ = self.checker.type_pool.get(type_id) orelse return;

        if (typ.kind == .@"enum") {
            for (typ.kind.@"enum".variants) |variant| {
                try items.append(self.allocator, .{
                    .label = variant.name,
                    .kind = .variant,
                    .detail = if (variant.payload != invalid_type)
                        self.checker.type_pool.format(variant.payload)
                    else
                        null,
                    .documentation = null,
                    .insert_text = null,
                    .sort_text = null,
                    .deprecated = false,
                });
            }
        }
    }

    /// Add type completions (struct, enum, trait names, primitives).
    fn addTypeCompletions(
        self: *CompletionEngine,
        items: *std.ArrayListUnmanaged(CompletionItem),
    ) !void {
        // Add primitive types.
        const primitives = [_][]const u8{
            "bool", "i8", "i16", "i32", "i64", "i128", "isize",
            "u8", "u16", "u32", "u64", "u128", "usize",
            "f32", "f64", "char", "str", "void",
        };

        for (primitives) |prim| {
            try items.append(self.allocator, .{
                .label = prim,
                .kind = .type_alias,
                .detail = "primitive type",
                .documentation = null,
                .insert_text = null,
                .sort_text = "0", // Primitives first
                .deprecated = false,
            });
        }

        // Add user-defined types from scope.
        for (self.checker.scopes.items) |scope| {
            var iter = scope.symbols.iterator();
            while (iter.next()) |entry| {
                const sym = entry.value_ptr.*;
                switch (sym.kind) {
                    .@"struct", .@"enum", .trait, .type_alias, .generic_param => {
                        try items.append(self.allocator, .{
                            .label = sym.name,
                            .kind = CompletionItemKind.fromSymbolKind(sym.kind),
                            .detail = self.checker.type_pool.format(sym.type_id),
                            .documentation = null,
                            .insert_text = null,
                            .sort_text = "1", // User types after primitives
                            .deprecated = false,
                        });
                    },
                    else => {},
                }
            }
        }
    }

    /// Add completions from current scope chain.
    fn addScopeCompletions(
        self: *CompletionEngine,
        items: *std.ArrayListUnmanaged(CompletionItem),
        offset: usize,
    ) !void {
        _ = offset;

        // Add all symbols from all scopes (inner to outer priority handled by sort).
        for (self.checker.scopes.items, 0..) |scope, scope_idx| {
            var iter = scope.symbols.iterator();
            while (iter.next()) |entry| {
                const sym = entry.value_ptr.*;

                // Create sort text based on scope depth (lower = inner = higher priority).
                var sort_buf: [8]u8 = undefined;
                const sort_text = std.fmt.bufPrint(&sort_buf, "{d}", .{scope_idx}) catch "9";
                const sort_copy = try self.allocator.dupe(u8, sort_text);

                try items.append(self.allocator, .{
                    .label = sym.name,
                    .kind = CompletionItemKind.fromSymbolKind(sym.kind),
                    .detail = self.checker.type_pool.format(sym.type_id),
                    .documentation = null,
                    .insert_text = null,
                    .sort_text = sort_copy,
                    .deprecated = false,
                });
            }
        }
    }

    /// Add pattern completions.
    fn addPatternCompletions(
        self: *CompletionEngine,
        items: *std.ArrayListUnmanaged(CompletionItem),
        offset: usize,
    ) !void {
        // Add wildcard pattern.
        try items.append(self.allocator, .{
            .label = "_",
            .kind = .keyword,
            .detail = "wildcard pattern",
            .documentation = "Matches any value",
            .insert_text = null,
            .sort_text = "0",
            .deprecated = false,
        });

        // Add enum variants from scope.
        try self.addScopeCompletions(items, offset);
    }

    /// Add module completions (for import statements).
    fn addModuleCompletions(
        self: *CompletionEngine,
        items: *std.ArrayListUnmanaged(CompletionItem),
    ) !void {
        // Add standard library modules.
        const std_modules = [_][]const u8{
            "std", "std.io", "std.fs", "std.mem", "std.fmt",
            "std.collections", "std.math", "std.string",
        };

        for (std_modules) |mod| {
            try items.append(self.allocator, .{
                .label = mod,
                .kind = .module,
                .detail = "module",
                .documentation = null,
                .insert_text = null,
                .sort_text = null,
                .deprecated = false,
            });
        }
    }

    /// Add keyword completions based on context.
    fn addKeywordCompletions(
        self: *CompletionEngine,
        items: *std.ArrayListUnmanaged(CompletionItem),
        context: CompletionContext,
    ) !void {
        const keywords = switch (context) {
            .statement => &[_][]const u8{
                "let",     "var",     "const",   "fn",        "struct",
                "enum",    "trait",   "impl",    "type",      "import",
                "pub",     "if",      "else",    "match",     "while",
                "for",     "loop",    "return",  "break",     "continue",
            },
            .expression => &[_][]const u8{
                "if",    "match",  "true", "false",
                "null",  "self",   "not",  "and",
                "or",
            },
            else => &[_][]const u8{},
        };

        for (keywords) |kw| {
            try items.append(self.allocator, .{
                .label = kw,
                .kind = .keyword,
                .detail = "keyword",
                .documentation = null,
                .insert_text = null,
                .sort_text = "z", // Keywords last
                .deprecated = false,
            });
        }
    }

    /// Sort completions by relevance.
    fn sortCompletions(self: *CompletionEngine, items: []CompletionItem, offset: usize) void {
        _ = self;
        _ = offset;

        // Sort by sort_text, then by label.
        std.mem.sort(CompletionItem, items, {}, struct {
            fn lessThan(_: void, a: CompletionItem, b: CompletionItem) bool {
                // Compare sort_text first.
                const a_sort = a.sort_text orelse "m";
                const b_sort = b.sort_text orelse "m";
                const cmp = std.mem.order(u8, a_sort, b_sort);
                if (cmp != .eq) return cmp == .lt;

                // Then by label.
                return std.mem.order(u8, a.label, b.label) == .lt;
            }
        }.lessThan);
    }
};

/// Calculate byte offset from line and column (both 1-indexed).
pub fn calculateOffset(source: []const u8, target_line: u32, target_column: u32) usize {
    var current_line: u32 = 1;
    var offset: usize = 0;

    while (offset < source.len and current_line < target_line) {
        if (source[offset] == '\n') {
            current_line += 1;
        }
        offset += 1;
    }

    // Now add column offset (1-indexed, so subtract 1).
    const col_offset = if (target_column > 0) target_column - 1 else 0;
    offset += col_offset;

    if (offset > source.len) {
        offset = source.len;
    }

    return offset;
}

/// Filter completions by prefix.
pub fn filterByPrefix(
    allocator: Allocator,
    items: []const CompletionItem,
    prefix: []const u8,
) ![]CompletionItem {
    var filtered = std.ArrayListUnmanaged(CompletionItem){};

    for (items) |item| {
        if (std.mem.startsWith(u8, item.label, prefix)) {
            try filtered.append(allocator, item);
        }
    }

    return try allocator.dupe(CompletionItem, filtered.items);
}

// === Tests ===

test "CompletionItemKind to LSP kind" {
    try std.testing.expectEqual(@as(u8, 3), CompletionItemKind.function.toLspKind());
    try std.testing.expectEqual(@as(u8, 6), CompletionItemKind.variable.toLspKind());
    try std.testing.expectEqual(@as(u8, 14), CompletionItemKind.keyword.toLspKind());
}

test "calculateOffset basic" {
    const source = "let x = 42\nlet y = 10\n";

    // Line 1, column 1 -> offset 0
    try std.testing.expectEqual(@as(usize, 0), calculateOffset(source, 1, 1));

    // Line 1, column 5 -> offset 4 (position of 'x')
    try std.testing.expectEqual(@as(usize, 4), calculateOffset(source, 1, 5));

    // Line 2, column 1 -> offset 11 (start of second line)
    try std.testing.expectEqual(@as(usize, 11), calculateOffset(source, 2, 1));
}

test "CompletionEngine analyzeContext - dot access" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "point.";

    // Create minimal AST and checker for testing.
    var tree = Ast.init(allocator, source, 0);
    defer tree.deinit();

    var checker_inst = try Checker.init(allocator, &tree);
    defer checker_inst.deinit();

    var engine = CompletionEngine.init(allocator, &checker_inst, &tree, source);

    const context = engine.analyzeContext(source.len);
    try std.testing.expectEqual(CompletionContext.dot_access, context);
}

test "CompletionEngine analyzeContext - type position after colon" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "let x: ";

    var tree = Ast.init(allocator, source, 0);
    defer tree.deinit();

    var checker_inst = try Checker.init(allocator, &tree);
    defer checker_inst.deinit();

    var engine = CompletionEngine.init(allocator, &checker_inst, &tree, source);

    const context = engine.analyzeContext(source.len);
    try std.testing.expectEqual(CompletionContext.type_position, context);
}

test "CompletionEngine analyzeContext - return type" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "fn foo() -> ";

    var tree = Ast.init(allocator, source, 0);
    defer tree.deinit();

    var checker_inst = try Checker.init(allocator, &tree);
    defer checker_inst.deinit();

    var engine = CompletionEngine.init(allocator, &checker_inst, &tree, source);

    const context = engine.analyzeContext(source.len);
    try std.testing.expectEqual(CompletionContext.type_position, context);
}

test "CompletionEngine analyzeContext - expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "let x = ";

    var tree = Ast.init(allocator, source, 0);
    defer tree.deinit();

    var checker_inst = try Checker.init(allocator, &tree);
    defer checker_inst.deinit();

    var engine = CompletionEngine.init(allocator, &checker_inst, &tree, source);

    const context = engine.analyzeContext(source.len);
    try std.testing.expectEqual(CompletionContext.expression, context);
}

test "filterByPrefix basic" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const items = [_]CompletionItem{
        .{ .label = "foo", .kind = .variable, .detail = null, .documentation = null, .insert_text = null, .sort_text = null, .deprecated = false },
        .{ .label = "bar", .kind = .variable, .detail = null, .documentation = null, .insert_text = null, .sort_text = null, .deprecated = false },
        .{ .label = "foobar", .kind = .function, .detail = null, .documentation = null, .insert_text = null, .sort_text = null, .deprecated = false },
    };

    const filtered = try filterByPrefix(allocator, &items, "foo");
    try std.testing.expectEqual(@as(usize, 2), filtered.len);
    try std.testing.expectEqualStrings("foo", filtered[0].label);
    try std.testing.expectEqualStrings("foobar", filtered[1].label);
}

test "CompletionEngine getCompletions - type position" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "let x: ";

    var tree = Ast.init(allocator, source, 0);
    defer tree.deinit();

    var checker_inst = try Checker.init(allocator, &tree);
    defer checker_inst.deinit();

    var engine = CompletionEngine.init(allocator, &checker_inst, &tree, source);
    const result = try engine.getCompletions(1, 8);

    // Should include primitive types.
    try std.testing.expectEqual(CompletionContext.type_position, result.context);
    try std.testing.expect(result.items.len > 0);

    // Find i32 in completions.
    var found_i32 = false;
    for (result.items) |item| {
        if (std.mem.eql(u8, item.label, "i32")) {
            found_i32 = true;
            break;
        }
    }
    try std.testing.expect(found_i32);
}

test "CompletionEngine getCompletions - expression with keywords" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "let x = ";

    var tree = Ast.init(allocator, source, 0);
    defer tree.deinit();

    var checker_inst = try Checker.init(allocator, &tree);
    defer checker_inst.deinit();

    var engine = CompletionEngine.init(allocator, &checker_inst, &tree, source);
    const result = try engine.getCompletions(1, 9);

    try std.testing.expectEqual(CompletionContext.expression, result.context);

    // Should include expression keywords.
    var found_true = false;
    var found_false = false;
    for (result.items) |item| {
        if (std.mem.eql(u8, item.label, "true")) found_true = true;
        if (std.mem.eql(u8, item.label, "false")) found_false = true;
    }
    try std.testing.expect(found_true);
    try std.testing.expect(found_false);
}

test "CompletionEngine analyzeContext - enum access" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "Color::";

    var tree = Ast.init(allocator, source, 0);
    defer tree.deinit();

    var checker_inst = try Checker.init(allocator, &tree);
    defer checker_inst.deinit();

    var engine = CompletionEngine.init(allocator, &checker_inst, &tree, source);

    const context = engine.analyzeContext(source.len);
    try std.testing.expectEqual(CompletionContext.enum_access, context);
}

test "CompletionItemKind from symbol kind conversion" {
    try std.testing.expectEqual(CompletionItemKind.variable, CompletionItemKind.fromSymbolKind(.variable));
    try std.testing.expectEqual(CompletionItemKind.function, CompletionItemKind.fromSymbolKind(.function));
    try std.testing.expectEqual(CompletionItemKind.@"struct", CompletionItemKind.fromSymbolKind(.@"struct"));
    try std.testing.expectEqual(CompletionItemKind.@"enum", CompletionItemKind.fromSymbolKind(.@"enum"));
    try std.testing.expectEqual(CompletionItemKind.field, CompletionItemKind.fromSymbolKind(.field));
    try std.testing.expectEqual(CompletionItemKind.variant, CompletionItemKind.fromSymbolKind(.variant));
}

test "CompletionEngine analyzeContext - statement start" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Empty file or start of line.
    const source = "";

    var tree = Ast.init(allocator, source, 0);
    defer tree.deinit();

    var checker_inst = try Checker.init(allocator, &tree);
    defer checker_inst.deinit();

    var engine = CompletionEngine.init(allocator, &checker_inst, &tree, source);

    const context = engine.analyzeContext(0);
    try std.testing.expectEqual(CompletionContext.statement, context);
}
