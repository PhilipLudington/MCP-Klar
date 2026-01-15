//! Symbol table builder that integrates with the type checker.
//!
//! This module provides functionality to build a comprehensive symbol table
//! during type checking, which can then be used for IDE-like features.

const std = @import("std");
const Allocator = std.mem.Allocator;
const compiler = @import("compiler");
const utils = @import("utils");
const symbols = @import("symbols.zig");

const ast = compiler.ast;
const types = compiler.types;
const checker = compiler.checker;

const Ast = ast.Ast;
const Node = ast.Node;
const NodeIndex = ast.NodeIndex;
const NodeRange = ast.NodeRange;
const null_node = ast.null_node;
const null_string = ast.null_string;

const TypeId = types.TypeId;
const TypePool = types.TypePool;
const BuiltinTypes = types.BuiltinTypes;
const invalid_type = types.invalid_type;

const Span = utils.Span;

const SymbolTable = symbols.SymbolTable;
const SymbolKind = symbols.SymbolKind;
const SymbolId = symbols.SymbolId;
const Scope = symbols.Scope;
const ScopeId = symbols.ScopeId;
const SymbolReference = symbols.SymbolReference;
const invalid_symbol = symbols.invalid_symbol;

/// Error type for symbol table building.
pub const BuildError = error{OutOfMemory};

/// Builds a symbol table from an AST and type pool.
pub const SymbolTableBuilder = struct {
    allocator: Allocator,
    tree: *const Ast,
    type_pool: *const TypePool,
    table: SymbolTable,
    /// Maps AST node indices to symbol IDs for cross-reference.
    node_to_symbol: std.AutoHashMapUnmanaged(NodeIndex, SymbolId),
    /// Current container symbol (e.g., struct being defined).
    current_container: SymbolId,
    /// File ID for span construction.
    file_id: u32,

    pub fn init(allocator: Allocator, tree: *const Ast, type_pool: *const TypePool, file_id: u32) SymbolTableBuilder {
        return .{
            .allocator = allocator,
            .tree = tree,
            .type_pool = type_pool,
            .table = SymbolTable.init(allocator),
            .node_to_symbol = .{},
            .current_container = invalid_symbol,
            .file_id = file_id,
        };
    }

    pub fn deinit(self: *SymbolTableBuilder) void {
        self.table.deinit();
        self.node_to_symbol.deinit(self.allocator);
    }

    /// Build the symbol table from the AST.
    pub fn build(self: *SymbolTableBuilder) BuildError!void {
        if (self.tree.root == null_node) return;

        // Push global scope.
        _ = try self.table.pushScope(.global, Span.empty);

        try self.visitNode(self.tree.root);
    }

    /// Get the built symbol table. Ownership transfers to caller.
    pub fn finish(self: *SymbolTableBuilder) SymbolTable {
        const result = self.table;
        // Reset to empty state so deinit doesn't double-free.
        self.table = SymbolTable.init(self.allocator);
        self.node_to_symbol.deinit(self.allocator);
        self.node_to_symbol = .{};
        return result;
    }

    /// Visit an AST node and collect symbols.
    fn visitNode(self: *SymbolTableBuilder, index: NodeIndex) BuildError!void {
        const node = self.tree.getNode(index) orelse return;

        switch (node.tag) {
            // Module level.
            .module => try self.visitModule(node),

            // Declarations.
            .fn_decl => try self.visitFnDecl(index, node),
            .struct_decl => try self.visitStructDecl(index, node),
            .enum_decl => try self.visitEnumDecl(index, node),
            .trait_decl => try self.visitTraitDecl(index, node),
            .impl_decl => try self.visitImplDecl(index, node),
            .type_alias => try self.visitTypeAlias(index, node),

            // Statements.
            .let_stmt => try self.visitLetStmt(index, node, false),
            .var_stmt => try self.visitLetStmt(index, node, true),
            .const_stmt => try self.visitConstStmt(index, node),
            .for_stmt => try self.visitForStmt(index, node),
            .while_stmt => try self.visitWhileStmt(node),
            .loop_stmt => try self.visitLoopStmt(node),
            .expr_stmt => try self.visitExprStmt(node),
            .return_stmt => try self.visitReturnStmt(node),

            // Expressions.
            .block => try self.visitBlock(node),
            .if_expr => try self.visitIfExpr(node),
            .match_expr => try self.visitMatchExpr(node),
            .identifier => try self.visitIdentifier(index, node),
            .lambda => try self.visitLambda(index, node),

            else => {},
        }
    }

    fn visitModule(self: *SymbolTableBuilder, node: Node) BuildError!void {
        const range = node.data.range;
        for (range.start..range.end) |i| {
            try self.visitNode(@intCast(i));
        }
    }

    fn visitFnDecl(self: *SymbolTableBuilder, index: NodeIndex, node: Node) BuildError!void {
        const data = node.data.fn_decl;
        const name = self.tree.getString(data.name) orelse return;

        // Add function symbol.
        const fn_symbol = try self.table.addSymbol(
            name,
            .function,
            invalid_type, // Type would come from checker
            node.span,
            index,
            data.is_pub,
            false,
            self.current_container,
        );

        try self.node_to_symbol.put(self.allocator, index, fn_symbol);

        // Skip if no body (trait method signature).
        if (data.body == null_node) return;

        // Push function scope.
        _ = try self.table.pushScope(.function, node.span);
        defer self.table.popScope();

        // Add parameters.
        for (data.params.start..data.params.end) |i| {
            const param = self.tree.getNode(@intCast(i)) orelse continue;
            if (param.tag == .param) {
                const param_data = param.data.param;
                if (self.tree.getString(param_data.name)) |param_name| {
                    _ = try self.table.addSymbol(
                        param_name,
                        .parameter,
                        invalid_type,
                        param.span,
                        @intCast(i),
                        false,
                        param_data.is_mut,
                        fn_symbol,
                    );
                }
            }
        }

        // Visit body.
        try self.visitNode(data.body);
    }

    fn visitStructDecl(self: *SymbolTableBuilder, index: NodeIndex, node: Node) BuildError!void {
        const data = node.data.struct_decl;
        const name = self.tree.getString(data.name) orelse return;

        // Add struct symbol.
        const struct_symbol = try self.table.addSymbol(
            name,
            .@"struct",
            invalid_type,
            node.span,
            index,
            data.is_pub,
            false,
            self.current_container,
        );

        try self.node_to_symbol.put(self.allocator, index, struct_symbol);

        // Push struct scope.
        const prev_container = self.current_container;
        self.current_container = struct_symbol;
        _ = try self.table.pushScope(.struct_def, node.span);
        defer {
            self.table.popScope();
            self.current_container = prev_container;
        }

        // Add generic parameters.
        for (data.generics.start..data.generics.end) |i| {
            const gp = self.tree.getNode(@intCast(i)) orelse continue;
            if (gp.tag == .generic_param) {
                if (self.tree.getString(gp.data.string)) |gp_name| {
                    _ = try self.table.addSymbol(
                        gp_name,
                        .generic_param,
                        invalid_type,
                        gp.span,
                        @intCast(i),
                        false,
                        false,
                        struct_symbol,
                    );
                }
            }
        }

        // Add fields.
        for (data.fields.start..data.fields.end) |i| {
            const field = self.tree.getNode(@intCast(i)) orelse continue;
            if (field.tag == .struct_field) {
                const field_data = field.data.field;
                if (self.tree.getString(field_data.name)) |field_name| {
                    _ = try self.table.addSymbol(
                        field_name,
                        .field,
                        invalid_type,
                        field.span,
                        @intCast(i),
                        field_data.is_pub,
                        false,
                        struct_symbol,
                    );
                }
            }
        }
    }

    fn visitEnumDecl(self: *SymbolTableBuilder, index: NodeIndex, node: Node) BuildError!void {
        const data = node.data.enum_decl;
        const name = self.tree.getString(data.name) orelse return;

        // Add enum symbol.
        const enum_symbol = try self.table.addSymbol(
            name,
            .@"enum",
            invalid_type,
            node.span,
            index,
            data.is_pub,
            false,
            self.current_container,
        );

        try self.node_to_symbol.put(self.allocator, index, enum_symbol);

        // Push enum scope.
        const prev_container = self.current_container;
        self.current_container = enum_symbol;
        _ = try self.table.pushScope(.enum_def, node.span);
        defer {
            self.table.popScope();
            self.current_container = prev_container;
        }

        // Add generic parameters.
        for (data.generics.start..data.generics.end) |i| {
            const gp = self.tree.getNode(@intCast(i)) orelse continue;
            if (gp.tag == .generic_param) {
                if (self.tree.getString(gp.data.string)) |gp_name| {
                    _ = try self.table.addSymbol(
                        gp_name,
                        .generic_param,
                        invalid_type,
                        gp.span,
                        @intCast(i),
                        false,
                        false,
                        enum_symbol,
                    );
                }
            }
        }

        // Add variants.
        for (data.variants.start..data.variants.end) |i| {
            const variant = self.tree.getNode(@intCast(i)) orelse continue;
            if (variant.tag == .enum_variant) {
                const variant_data = variant.data.variant;
                if (self.tree.getString(variant_data.name)) |variant_name| {
                    _ = try self.table.addSymbol(
                        variant_name,
                        .variant,
                        invalid_type,
                        variant.span,
                        @intCast(i),
                        true, // Variants are implicitly public
                        false,
                        enum_symbol,
                    );
                }
            }
        }
    }

    fn visitTraitDecl(self: *SymbolTableBuilder, index: NodeIndex, node: Node) BuildError!void {
        const data = node.data.trait_decl;
        const name = self.tree.getString(data.name) orelse return;

        // Add trait symbol.
        const trait_symbol = try self.table.addSymbol(
            name,
            .trait,
            invalid_type,
            node.span,
            index,
            data.is_pub,
            false,
            self.current_container,
        );

        try self.node_to_symbol.put(self.allocator, index, trait_symbol);

        // Push trait scope.
        const prev_container = self.current_container;
        self.current_container = trait_symbol;
        _ = try self.table.pushScope(.trait_def, node.span);
        defer {
            self.table.popScope();
            self.current_container = prev_container;
        }

        // Add methods.
        for (data.methods.start..data.methods.end) |i| {
            const method = self.tree.getNode(@intCast(i)) orelse continue;
            if (method.tag == .fn_decl) {
                try self.visitFnDecl(@intCast(i), method);
            }
        }
    }

    fn visitImplDecl(self: *SymbolTableBuilder, index: NodeIndex, node: Node) BuildError!void {
        const data = node.data.impl_decl;

        // Add impl block symbol (anonymous).
        const impl_symbol = try self.table.addSymbol(
            "<impl>",
            .impl_block,
            invalid_type,
            node.span,
            index,
            false,
            false,
            self.current_container,
        );

        // Push impl scope.
        const prev_container = self.current_container;
        self.current_container = impl_symbol;
        _ = try self.table.pushScope(.impl_block, node.span);
        defer {
            self.table.popScope();
            self.current_container = prev_container;
        }

        // Add methods.
        for (data.methods.start..data.methods.end) |i| {
            const method = self.tree.getNode(@intCast(i)) orelse continue;
            if (method.tag == .fn_decl) {
                // Mark as method instead of function.
                const method_data = method.data.fn_decl;
                if (self.tree.getString(method_data.name)) |method_name| {
                    const method_symbol = try self.table.addSymbol(
                        method_name,
                        .method,
                        invalid_type,
                        method.span,
                        @intCast(i),
                        method_data.is_pub,
                        false,
                        impl_symbol,
                    );

                    // Visit method body if present.
                    if (method_data.body != null_node) {
                        _ = try self.table.pushScope(.function, method.span);
                        defer self.table.popScope();

                        // Add parameters.
                        for (method_data.params.start..method_data.params.end) |j| {
                            const param = self.tree.getNode(@intCast(j)) orelse continue;
                            if (param.tag == .param) {
                                const param_data = param.data.param;
                                if (self.tree.getString(param_data.name)) |param_name| {
                                    _ = try self.table.addSymbol(
                                        param_name,
                                        .parameter,
                                        invalid_type,
                                        param.span,
                                        @intCast(j),
                                        false,
                                        param_data.is_mut,
                                        method_symbol,
                                    );
                                }
                            }
                        }

                        try self.visitNode(method_data.body);
                    }
                }
            }
        }
    }

    fn visitTypeAlias(self: *SymbolTableBuilder, index: NodeIndex, node: Node) BuildError!void {
        const data = node.data.type_alias;
        const name = self.tree.getString(data.name) orelse return;

        const alias_symbol = try self.table.addSymbol(
            name,
            .type_alias,
            invalid_type,
            node.span,
            index,
            data.is_pub,
            false,
            self.current_container,
        );

        try self.node_to_symbol.put(self.allocator, index, alias_symbol);
    }

    fn visitLetStmt(self: *SymbolTableBuilder, index: NodeIndex, node: Node, is_mutable: bool) BuildError!void {
        const data = node.data.let;
        const name = self.tree.getString(data.name) orelse return;

        _ = try self.table.addSymbol(
            name,
            if (is_mutable) .mutable_variable else .variable,
            invalid_type,
            node.span,
            index,
            false,
            is_mutable,
            self.current_container,
        );

        // Visit initializer for nested symbols.
        if (data.value != null_node) {
            try self.visitNode(data.value);
        }
    }

    fn visitConstStmt(self: *SymbolTableBuilder, index: NodeIndex, node: Node) BuildError!void {
        const data = node.data.let;
        const name = self.tree.getString(data.name) orelse return;

        _ = try self.table.addSymbol(
            name,
            .constant,
            invalid_type,
            node.span,
            index,
            false,
            false,
            self.current_container,
        );

        if (data.value != null_node) {
            try self.visitNode(data.value);
        }
    }

    fn visitForStmt(self: *SymbolTableBuilder, index: NodeIndex, node: Node) BuildError!void {
        const data = node.data.for_loop;

        // Visit iterator first (outside loop scope).
        if (data.iterator != null_node) {
            try self.visitNode(data.iterator);
        }

        // Push loop scope.
        _ = try self.table.pushScope(.loop, node.span);
        defer self.table.popScope();

        // Add binding.
        if (self.tree.getString(data.binding)) |binding_name| {
            _ = try self.table.addSymbol(
                binding_name,
                .variable,
                invalid_type,
                node.span,
                index,
                false,
                false,
                invalid_symbol,
            );
        }

        // Visit body.
        if (data.body != null_node) {
            try self.visitNode(data.body);
        }
    }

    fn visitWhileStmt(self: *SymbolTableBuilder, node: Node) BuildError!void {
        const data = node.data.while_loop;

        // Visit condition.
        if (data.condition != null_node) {
            try self.visitNode(data.condition);
        }

        // Push loop scope.
        _ = try self.table.pushScope(.loop, node.span);
        defer self.table.popScope();

        // Visit body.
        if (data.body != null_node) {
            try self.visitNode(data.body);
        }
    }

    fn visitLoopStmt(self: *SymbolTableBuilder, node: Node) BuildError!void {
        // Push loop scope.
        _ = try self.table.pushScope(.loop, node.span);
        defer self.table.popScope();

        // Visit body.
        if (node.data.node != null_node) {
            try self.visitNode(node.data.node);
        }
    }

    fn visitExprStmt(self: *SymbolTableBuilder, node: Node) BuildError!void {
        if (node.data.node != null_node) {
            try self.visitNode(node.data.node);
        }
    }

    fn visitReturnStmt(self: *SymbolTableBuilder, node: Node) BuildError!void {
        if (node.data.node != null_node) {
            try self.visitNode(node.data.node);
        }
    }

    fn visitBlock(self: *SymbolTableBuilder, node: Node) BuildError!void {
        const range = node.data.range;

        // Push block scope.
        _ = try self.table.pushScope(.block, node.span);
        defer self.table.popScope();

        for (range.start..range.end) |i| {
            try self.visitNode(@intCast(i));
        }
    }

    fn visitIfExpr(self: *SymbolTableBuilder, node: Node) BuildError!void {
        const data = node.data.if_expr;

        if (data.condition != null_node) {
            try self.visitNode(data.condition);
        }
        if (data.then_block != null_node) {
            try self.visitNode(data.then_block);
        }
        if (data.else_block != null_node) {
            try self.visitNode(data.else_block);
        }
    }

    fn visitMatchExpr(self: *SymbolTableBuilder, node: Node) BuildError!void {
        const data = node.data.match_expr;

        if (data.value != null_node) {
            try self.visitNode(data.value);
        }

        // Visit arms.
        for (data.arms.start..data.arms.end) |i| {
            const arm = self.tree.getNode(@intCast(i)) orelse continue;
            if (arm.tag == .match_arm) {
                // Push match arm scope for pattern bindings.
                _ = try self.table.pushScope(.match_arm, arm.span);
                defer self.table.popScope();

                // TODO: Visit pattern and extract bindings.
                const arm_data = arm.data.match_arm;
                if (arm_data.body != null_node) {
                    try self.visitNode(arm_data.body);
                }
            }
        }
    }

    fn visitIdentifier(self: *SymbolTableBuilder, index: NodeIndex, node: Node) BuildError!void {
        const name = self.tree.getString(node.data.string) orelse return;

        // Look up the symbol and add a reference.
        if (self.table.lookup(name)) |symbol_id| {
            try self.table.addReference(symbol_id, node.span, index, .read);
        }
    }

    fn visitLambda(self: *SymbolTableBuilder, index: NodeIndex, node: Node) BuildError!void {
        const data = node.data.lambda;

        // Push lambda scope.
        _ = try self.table.pushScope(.lambda, node.span);
        defer self.table.popScope();

        // Add parameters.
        for (data.params.start..data.params.end) |i| {
            const param = self.tree.getNode(@intCast(i)) orelse continue;
            if (param.tag == .param) {
                const param_data = param.data.param;
                if (self.tree.getString(param_data.name)) |param_name| {
                    _ = try self.table.addSymbol(
                        param_name,
                        .parameter,
                        invalid_type,
                        param.span,
                        @intCast(i),
                        false,
                        param_data.is_mut,
                        invalid_symbol,
                    );
                }
            }
        }

        try self.node_to_symbol.put(self.allocator, index, invalid_symbol);

        // Visit body.
        if (data.body != null_node) {
            try self.visitNode(data.body);
        }
    }
};

/// Build a symbol table from an AST.
pub fn buildSymbolTable(
    allocator: Allocator,
    tree: *const Ast,
    type_pool: *const TypePool,
    file_id: u32,
) !SymbolTable {
    var builder = SymbolTableBuilder.init(allocator, tree, type_pool, file_id);
    errdefer builder.deinit();

    try builder.build();
    return builder.finish();
}

// === Tests ===

const Parser = compiler.parser.Parser;

fn parseSource(allocator: Allocator, source: []const u8) !Ast {
    var p = Parser.init(allocator, source, 0);
    defer p.deinit();
    return p.parse();
}

test "SymbolTableBuilder empty source" {
    var tree = try parseSource(std.testing.allocator, "");
    defer tree.deinit();

    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    var table = try buildSymbolTable(std.testing.allocator, &tree, &pool, 0);
    defer table.deinit();

    // Should have at least the global scope.
    try std.testing.expectEqual(@as(usize, 1), table.scopeCount());
}

test "SymbolTableBuilder function declaration" {
    const source =
        \\fn add(a: i32, b: i32) -> i32 {
        \\    a + b
        \\}
    ;

    var tree = try parseSource(std.testing.allocator, source);
    defer tree.deinit();

    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    var table = try buildSymbolTable(std.testing.allocator, &tree, &pool, 0);
    defer table.deinit();

    // Should have: function 'add', params 'a' and 'b'.
    // Find the function symbol.
    const func = table.lookup("add");
    try std.testing.expect(func != null);

    const func_sym = table.getSymbol(func.?).?;
    try std.testing.expect(func_sym.kind == .function);
    try std.testing.expect(std.mem.eql(u8, func_sym.name, "add"));
}

test "SymbolTableBuilder struct declaration" {
    const source =
        \\struct Point {
        \\    x: i32,
        \\    y: i32
        \\}
    ;

    var tree = try parseSource(std.testing.allocator, source);
    defer tree.deinit();

    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    var table = try buildSymbolTable(std.testing.allocator, &tree, &pool, 0);
    defer table.deinit();

    // Find Point.
    const point = table.lookup("Point");
    try std.testing.expect(point != null);

    const point_sym = table.getSymbol(point.?).?;
    try std.testing.expect(point_sym.kind == .@"struct");
}

test "SymbolTableBuilder let binding" {
    const source = "let x = 42";

    var tree = try parseSource(std.testing.allocator, source);
    defer tree.deinit();

    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    var table = try buildSymbolTable(std.testing.allocator, &tree, &pool, 0);
    defer table.deinit();

    const x = table.lookup("x");
    try std.testing.expect(x != null);

    const x_sym = table.getSymbol(x.?).?;
    try std.testing.expect(x_sym.kind == .variable);
    try std.testing.expect(!x_sym.is_mutable);
}

test "SymbolTableBuilder var binding" {
    const source = "var y = 123";

    var tree = try parseSource(std.testing.allocator, source);
    defer tree.deinit();

    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    var table = try buildSymbolTable(std.testing.allocator, &tree, &pool, 0);
    defer table.deinit();

    const y = table.lookup("y");
    try std.testing.expect(y != null);

    const y_sym = table.getSymbol(y.?).?;
    try std.testing.expect(y_sym.kind == .mutable_variable);
    try std.testing.expect(y_sym.is_mutable);
}

test "SymbolTableBuilder nested scopes" {
    const source =
        \\fn outer() {
        \\    let x = 1
        \\    {
        \\        let y = 2
        \\    }
        \\}
    ;

    var tree = try parseSource(std.testing.allocator, source);
    defer tree.deinit();

    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    var table = try buildSymbolTable(std.testing.allocator, &tree, &pool, 0);
    defer table.deinit();

    // Should have: global, function, block scopes.
    try std.testing.expect(table.scopeCount() >= 3);
}

test "SymbolTableBuilder enum with variants" {
    const source =
        \\enum Color {
        \\    Red,
        \\    Green,
        \\    Blue
        \\}
    ;

    var tree = try parseSource(std.testing.allocator, source);
    defer tree.deinit();

    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    var table = try buildSymbolTable(std.testing.allocator, &tree, &pool, 0);
    defer table.deinit();

    const color = table.lookup("Color");
    try std.testing.expect(color != null);

    const color_sym = table.getSymbol(color.?).?;
    try std.testing.expect(color_sym.kind == .@"enum");
}
