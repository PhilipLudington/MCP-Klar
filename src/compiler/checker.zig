//! Type checker for the Klar language.
//!
//! This module performs semantic analysis on the AST, including:
//! - Type resolution from type annotations
//! - Type inference for variables without annotations
//! - Type checking for expressions and statements
//! - Function signature verification
//! - Struct and enum type validation

const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const types = @import("types.zig");
const utils = @import("utils");

const Ast = ast.Ast;
const Node = ast.Node;
const NodeIndex = ast.NodeIndex;
const NodeRange = ast.NodeRange;
const StringIndex = ast.StringIndex;
const null_node = ast.null_node;
const null_string = ast.null_string;
const BinaryOp = ast.BinaryOp;
const UnaryOp = ast.UnaryOp;

const Type = types.Type;
const TypeId = types.TypeId;
const TypePool = types.TypePool;
const Primitive = types.Primitive;
const BuiltinTypes = types.BuiltinTypes;
const invalid_type = types.invalid_type;
const TypeCompat = types.TypeCompat;
const checkCompatibility = types.checkCompatibility;

const Span = utils.Span;
const Diagnostic = utils.Diagnostic;
const DiagnosticList = utils.diagnostic.DiagnosticList;

/// Error set for type checking operations.
pub const CheckerError = error{OutOfMemory};

/// Symbol in the symbol table.
pub const Symbol = struct {
    name: []const u8,
    type_id: TypeId,
    kind: Kind,
    is_mutable: bool,
    span: Span,

    pub const Kind = enum {
        variable,
        parameter,
        function,
        @"struct",
        @"enum",
        trait,
        type_alias,
        generic_param,
        field,
        variant,
    };
};

/// Scope for symbol lookup.
pub const Scope = struct {
    symbols: std.StringHashMapUnmanaged(Symbol),
    parent: ?*Scope,

    pub fn init() Scope {
        return .{
            .symbols = .{},
            .parent = null,
        };
    }

    pub fn deinit(self: *Scope, allocator: Allocator) void {
        self.symbols.deinit(allocator);
    }

    pub fn define(self: *Scope, allocator: Allocator, name: []const u8, symbol: Symbol) CheckerError!void {
        try self.symbols.put(allocator, name, symbol);
    }

    pub fn lookup(self: *const Scope, name: []const u8) ?Symbol {
        if (self.symbols.get(name)) |sym| {
            return sym;
        }
        if (self.parent) |p| {
            return p.lookup(name);
        }
        return null;
    }

    pub fn lookupLocal(self: *const Scope, name: []const u8) ?Symbol {
        return self.symbols.get(name);
    }
};

/// Type checker state.
pub const Checker = struct {
    allocator: Allocator,
    tree: *const Ast,
    type_pool: TypePool,
    diagnostics: DiagnosticList,

    /// Scope stack for nested scopes.
    scopes: std.ArrayListUnmanaged(Scope),

    /// Current function return type (for checking return statements).
    current_return_type: TypeId,

    /// Expression type cache: maps node indices to their resolved types.
    expr_types: std.AutoHashMapUnmanaged(NodeIndex, TypeId),

    /// Self type for impl blocks.
    self_type: TypeId,

    pub fn init(allocator: Allocator, tree: *const Ast) !Checker {
        var checker = Checker{
            .allocator = allocator,
            .tree = tree,
            .type_pool = try TypePool.init(allocator),
            .diagnostics = DiagnosticList.init(allocator),
            .scopes = .{},
            .current_return_type = BuiltinTypes.void_type,
            .expr_types = .{},
            .self_type = invalid_type,
        };

        // Push global scope.
        try checker.pushScope();

        return checker;
    }

    pub fn deinit(self: *Checker) void {
        for (self.scopes.items) |*scope| {
            scope.deinit(self.allocator);
        }
        self.scopes.deinit(self.allocator);
        self.type_pool.deinit();

        // Free diagnostic messages we allocated.
        for (self.diagnostics.items.items) |diag| {
            self.allocator.free(diag.message);
        }
        self.diagnostics.deinit();

        self.expr_types.deinit(self.allocator);
    }

    /// Run the type checker on the AST.
    pub fn check(self: *Checker) CheckerError!void {
        if (self.tree.root == null_node) return;
        try self.checkNode(self.tree.root);
    }

    /// Check a single node.
    fn checkNode(self: *Checker, index: NodeIndex) CheckerError!void {
        const node = self.tree.getNode(index) orelse return;

        switch (node.tag) {
            // Module level
            .module => try self.checkModule(node),

            // Declarations
            .fn_decl => try self.checkFnDecl(node),
            .struct_decl => try self.checkStructDecl(node),
            .enum_decl => try self.checkEnumDecl(node),
            .trait_decl => try self.checkTraitDecl(node),
            .impl_decl => try self.checkImplDecl(node),
            .type_alias => try self.checkTypeAlias(node),

            // Statements
            .let_stmt, .var_stmt => try self.checkLetStmt(node),
            .const_stmt => try self.checkConstStmt(node),
            .return_stmt => try self.checkReturnStmt(node),
            .while_stmt => try self.checkWhileStmt(node),
            .for_stmt => try self.checkForStmt(node),
            .loop_stmt => try self.checkLoopStmt(node),
            .expr_stmt => try self.checkExprStmt(node),
            .break_stmt, .continue_stmt => {},

            // Import
            .import_stmt => {}, // TODO: implement import resolution

            else => {},
        }
    }

    /// Check a module (top-level).
    fn checkModule(self: *Checker, node: Node) CheckerError!void {
        const range = node.data.range;
        // First pass: collect declarations.
        for (range.start..range.end) |i| {
            const child = self.tree.getNode(@intCast(i)) orelse continue;
            try self.collectDeclaration(@intCast(i), child);
        }

        // Second pass: check all declarations.
        for (range.start..range.end) |i| {
            try self.checkNode(@intCast(i));
        }
    }

    /// Collect a declaration into the current scope (first pass).
    fn collectDeclaration(self: *Checker, index: NodeIndex, node: Node) CheckerError!void {
        switch (node.tag) {
            .fn_decl => {
                const data = node.data.fn_decl;
                if (self.tree.getString(data.name)) |name| {
                    // Create function type.
                    const fn_type = try self.resolveFnType(data);
                    try self.defineSymbol(name, .{
                        .name = name,
                        .type_id = fn_type,
                        .kind = .function,
                        .is_mutable = false,
                        .span = node.span,
                    });
                }
            },
            .struct_decl => {
                const data = node.data.struct_decl;
                if (self.tree.getString(data.name)) |name| {
                    const struct_type = try self.registerStructType(index, name, data);
                    try self.defineSymbol(name, .{
                        .name = name,
                        .type_id = struct_type,
                        .kind = .@"struct",
                        .is_mutable = false,
                        .span = node.span,
                    });
                }
            },
            .enum_decl => {
                const data = node.data.enum_decl;
                if (self.tree.getString(data.name)) |name| {
                    const enum_type = try self.registerEnumType(index, name, data);
                    try self.defineSymbol(name, .{
                        .name = name,
                        .type_id = enum_type,
                        .kind = .@"enum",
                        .is_mutable = false,
                        .span = node.span,
                    });
                }
            },
            .trait_decl => {
                const data = node.data.trait_decl;
                if (self.tree.getString(data.name)) |name| {
                    const trait_type = try self.registerTraitType(name, data);
                    try self.defineSymbol(name, .{
                        .name = name,
                        .type_id = trait_type,
                        .kind = .trait,
                        .is_mutable = false,
                        .span = node.span,
                    });
                }
            },
            .type_alias => {
                const data = node.data.type_alias;
                if (self.tree.getString(data.name)) |name| {
                    // Type aliases resolved lazily.
                    try self.defineSymbol(name, .{
                        .name = name,
                        .type_id = invalid_type, // Resolved later
                        .kind = .type_alias,
                        .is_mutable = false,
                        .span = node.span,
                    });
                }
            },
            else => {},
        }
    }

    /// Check a function declaration.
    fn checkFnDecl(self: *Checker, node: Node) CheckerError!void {
        const data = node.data.fn_decl;

        // Skip if no body (trait method signature).
        if (data.body == null_node) return;

        // Push function scope.
        try self.pushScope();
        defer self.popScope();

        // Add parameters to scope.
        for (data.params.start..data.params.end) |i| {
            const param = self.tree.getNode(@intCast(i)) orelse continue;
            if (param.tag == .param) {
                const param_data = param.data.param;
                if (self.tree.getString(param_data.name)) |name| {
                    const param_type = try self.resolveTypeNode(param_data.type_node);
                    try self.defineSymbol(name, .{
                        .name = name,
                        .type_id = param_type,
                        .kind = .parameter,
                        .is_mutable = param_data.is_mut,
                        .span = param.span,
                    });
                }
            }
        }

        // Set return type for checking return statements.
        const return_type = if (data.return_type != null_node)
            try self.resolveTypeNode(data.return_type)
        else
            BuiltinTypes.void_type;

        const prev_return_type = self.current_return_type;
        self.current_return_type = return_type;
        defer self.current_return_type = prev_return_type;

        // Check body.
        _ = try self.checkExpr(data.body);
    }

    /// Check a struct declaration.
    fn checkStructDecl(self: *Checker, node: Node) CheckerError!void {
        const data = node.data.struct_decl;

        // Check field types are valid.
        for (data.fields.start..data.fields.end) |i| {
            const field = self.tree.getNode(@intCast(i)) orelse continue;
            if (field.tag == .struct_field) {
                const field_data = field.data.field;
                _ = try self.resolveTypeNode(field_data.type_node);
            }
        }
    }

    /// Check an enum declaration.
    fn checkEnumDecl(self: *Checker, node: Node) CheckerError!void {
        const data = node.data.enum_decl;

        // Check variant payload types are valid.
        for (data.variants.start..data.variants.end) |i| {
            const variant = self.tree.getNode(@intCast(i)) orelse continue;
            if (variant.tag == .enum_variant) {
                const variant_data = variant.data.variant;
                if (variant_data.payload != null_node) {
                    _ = try self.resolveTypeNode(variant_data.payload);
                }
            }
        }
    }

    /// Check a trait declaration.
    fn checkTraitDecl(self: *Checker, node: Node) CheckerError!void {
        const data = node.data.trait_decl;

        // Check method signatures.
        for (data.methods.start..data.methods.end) |i| {
            const method = self.tree.getNode(@intCast(i)) orelse continue;
            if (method.tag == .fn_decl) {
                // Check parameter and return types.
                const method_data = method.data.fn_decl;
                for (method_data.params.start..method_data.params.end) |j| {
                    const param = self.tree.getNode(@intCast(j)) orelse continue;
                    if (param.tag == .param) {
                        _ = try self.resolveTypeNode(param.data.param.type_node);
                    }
                }
                if (method_data.return_type != null_node) {
                    _ = try self.resolveTypeNode(method_data.return_type);
                }
            }
        }
    }

    /// Check an impl block.
    fn checkImplDecl(self: *Checker, node: Node) CheckerError!void {
        const data = node.data.impl_decl;

        // Resolve the target type (Self).
        const target_type = try self.resolveTypeNode(data.target_type);
        const prev_self = self.self_type;
        self.self_type = target_type;
        defer self.self_type = prev_self;

        // Check each method.
        for (data.methods.start..data.methods.end) |i| {
            const method = self.tree.getNode(@intCast(i)) orelse continue;
            if (method.tag == .fn_decl) {
                try self.checkFnDecl(method);
            }
        }
    }

    /// Check a type alias.
    fn checkTypeAlias(self: *Checker, node: Node) CheckerError!void {
        const data = node.data.type_alias;
        _ = try self.resolveTypeNode(data.aliased_type);
    }

    /// Check a let/var statement.
    fn checkLetStmt(self: *Checker, node: Node) CheckerError!void {
        const data = node.data.let;
        const is_mutable = node.tag == .var_stmt;

        // Get the initializer type.
        const init_type = try self.checkExpr(data.value);

        // Get the declared type or infer from initializer.
        const declared_type = if (data.type_node != null_node)
            try self.resolveTypeNode(data.type_node)
        else
            init_type;

        // Check type compatibility.
        if (data.type_node != null_node) {
            const compat = checkCompatibility(&self.type_pool, init_type, declared_type);
            if (compat == .incompatible) {
                try self.emitError(
                    "type mismatch: expected {s}, found {s}",
                    .{ self.type_pool.format(declared_type), self.type_pool.format(init_type) },
                    node.span,
                );
            }
        }

        // Add to scope.
        if (self.tree.getString(data.name)) |name| {
            try self.defineSymbol(name, .{
                .name = name,
                .type_id = declared_type,
                .kind = .variable,
                .is_mutable = is_mutable,
                .span = node.span,
            });
        }
    }

    /// Check a const statement.
    fn checkConstStmt(self: *Checker, node: Node) CheckerError!void {
        const data = node.data.let;

        // Get the initializer type.
        const init_type = try self.checkExpr(data.value);

        // Get the declared type or infer from initializer.
        const declared_type = if (data.type_node != null_node)
            try self.resolveTypeNode(data.type_node)
        else
            init_type;

        // Add to scope.
        if (self.tree.getString(data.name)) |name| {
            try self.defineSymbol(name, .{
                .name = name,
                .type_id = declared_type,
                .kind = .variable,
                .is_mutable = false,
                .span = node.span,
            });
        }
    }

    /// Check a return statement.
    fn checkReturnStmt(self: *Checker, node: Node) CheckerError!void {
        const return_value = node.data.node;

        const actual_type = if (return_value != null_node)
            try self.checkExpr(return_value)
        else
            BuiltinTypes.void_type;

        const compat = checkCompatibility(&self.type_pool, actual_type, self.current_return_type);
        if (compat == .incompatible) {
            try self.emitError(
                "return type mismatch: expected {s}, found {s}",
                .{ self.type_pool.format(self.current_return_type), self.type_pool.format(actual_type) },
                node.span,
            );
        }
    }

    /// Check a while statement.
    fn checkWhileStmt(self: *Checker, node: Node) CheckerError!void {
        const data = node.data.while_loop;

        // Check condition is bool.
        const cond_type = try self.checkExpr(data.condition);
        if (cond_type != BuiltinTypes.bool_type) {
            try self.emitError(
                "while condition must be bool, found {s}",
                .{self.type_pool.format(cond_type)},
                node.span,
            );
        }

        // Check body.
        _ = try self.checkExpr(data.body);
    }

    /// Check a for statement.
    fn checkForStmt(self: *Checker, node: Node) CheckerError!void {
        const data = node.data.for_loop;

        // Check iterator expression.
        const iter_type = try self.checkExpr(data.iterator);
        _ = iter_type; // TODO: check iterable

        // Push scope for loop variable.
        try self.pushScope();
        defer self.popScope();

        // Add binding to scope.
        if (self.tree.getString(data.binding)) |name| {
            try self.defineSymbol(name, .{
                .name = name,
                .type_id = invalid_type, // TODO: infer from iterator
                .kind = .variable,
                .is_mutable = false,
                .span = node.span,
            });
        }

        // Check body.
        _ = try self.checkExpr(data.body);
    }

    /// Check a loop statement.
    fn checkLoopStmt(self: *Checker, node: Node) CheckerError!void {
        const body = node.data.node;
        if (body != null_node) {
            _ = try self.checkExpr(body);
        }
    }

    /// Check an expression statement.
    fn checkExprStmt(self: *Checker, node: Node) CheckerError!void {
        const expr = node.data.node;
        if (expr != null_node) {
            _ = try self.checkExpr(expr);
        }
    }

    /// Check an expression and return its type.
    pub fn checkExpr(self: *Checker, index: NodeIndex) CheckerError!TypeId {
        // Check cache first.
        if (self.expr_types.get(index)) |t| {
            return t;
        }

        const node = self.tree.getNode(index) orelse return invalid_type;
        const result = try self.checkExprInner(node, index);

        // Cache result.
        try self.expr_types.put(self.allocator, index, result);

        return result;
    }

    fn checkExprInner(self: *Checker, node: Node, index: NodeIndex) CheckerError!TypeId {
        _ = index;
        return switch (node.tag) {
            // Literals
            .integer_literal => BuiltinTypes.i32_type, // Default to i32
            .float_literal => BuiltinTypes.f64_type, // Default to f64
            .string_literal => BuiltinTypes.str_type,
            .char_literal => BuiltinTypes.char_type,
            .bool_literal => BuiltinTypes.bool_type,

            // Identifier
            .identifier => try self.checkIdentifier(node),

            // Self
            .self_expr => self.self_type,
            .self_type => self.self_type,

            // Operations
            .binary_op => try self.checkBinaryOp(node),
            .unary_op => try self.checkUnaryOp(node),

            // Access
            .field_access => try self.checkFieldAccess(node),
            .index_access => try self.checkIndexAccess(node),
            .call => try self.checkCall(node),
            .method_call => try self.checkMethodCall(node),

            // Control flow
            .if_expr => try self.checkIfExpr(node),
            .match_expr => try self.checkMatchExpr(node),
            .block => try self.checkBlock(node),
            .grouped => try self.checkGrouped(node),

            // Literals
            .array_literal => try self.checkArrayLiteral(node),
            .struct_literal => try self.checkStructLiteral(node),
            .lambda => try self.checkLambda(node),
            .range => try self.checkRange(node),

            // Error
            .@"error" => invalid_type,

            else => invalid_type,
        };
    }

    /// Check an identifier expression.
    fn checkIdentifier(self: *Checker, node: Node) CheckerError!TypeId {
        const name_idx = node.data.string;
        const name = self.tree.getString(name_idx) orelse return invalid_type;

        if (self.lookupSymbol(name)) |sym| {
            return sym.type_id;
        }

        try self.emitError("undefined identifier '{s}'", .{name}, node.span);
        return invalid_type;
    }

    /// Check a binary operation.
    fn checkBinaryOp(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.binary;
        const left_type = try self.checkExpr(data.left);
        const right_type = try self.checkExpr(data.right);

        return switch (data.op) {
            // Arithmetic operators require numeric types.
            .add, .sub, .mul, .div, .mod, .add_wrap, .sub_wrap, .mul_wrap, .add_sat, .sub_sat, .mul_sat => blk: {
                const left = self.type_pool.get(left_type);
                const right = self.type_pool.get(right_type);

                if (left == null or right == null) break :blk invalid_type;

                if (!left.?.isNumeric() or !right.?.isNumeric()) {
                    try self.emitError(
                        "arithmetic operation requires numeric operands",
                        .{},
                        node.span,
                    );
                    break :blk invalid_type;
                }

                // Result type is the "larger" numeric type.
                break :blk self.promoteNumeric(left_type, right_type);
            },

            // Comparison operators produce bool.
            .eq, .ne, .lt, .gt, .le, .ge => blk: {
                // Check operand compatibility.
                const compat = checkCompatibility(&self.type_pool, left_type, right_type);
                if (compat == .incompatible) {
                    const compat_rev = checkCompatibility(&self.type_pool, right_type, left_type);
                    if (compat_rev == .incompatible) {
                        try self.emitError(
                            "cannot compare {s} with {s}",
                            .{ self.type_pool.format(left_type), self.type_pool.format(right_type) },
                            node.span,
                        );
                    }
                }
                break :blk BuiltinTypes.bool_type;
            },

            // Logical operators require and produce bool.
            .@"and", .@"or" => blk: {
                if (left_type != BuiltinTypes.bool_type or right_type != BuiltinTypes.bool_type) {
                    try self.emitError(
                        "logical operation requires bool operands",
                        .{},
                        node.span,
                    );
                }
                break :blk BuiltinTypes.bool_type;
            },

            // Bitwise operators require integers.
            .bit_and, .bit_or, .bit_xor, .shl, .shr => blk: {
                const left = self.type_pool.get(left_type);
                const right = self.type_pool.get(right_type);

                if (left == null or right == null) break :blk invalid_type;

                if (!left.?.isInteger() or !right.?.isInteger()) {
                    try self.emitError(
                        "bitwise operation requires integer operands",
                        .{},
                        node.span,
                    );
                    break :blk invalid_type;
                }

                break :blk left_type;
            },

            // Null coalescing: ?T ?? T -> T
            .null_coalesce => blk: {
                const left = self.type_pool.get(left_type);
                if (left == null) break :blk invalid_type;

                if (left.?.kind != .optional) {
                    try self.emitError(
                        "null coalescing requires optional type on left",
                        .{},
                        node.span,
                    );
                    break :blk left_type;
                }

                break :blk left.?.kind.optional.inner;
            },

            // Assignment returns the assigned type.
            .assign, .add_assign, .sub_assign, .mul_assign, .div_assign, .mod_assign => blk: {
                const compat = checkCompatibility(&self.type_pool, right_type, left_type);
                if (compat == .incompatible) {
                    try self.emitError(
                        "cannot assign {s} to {s}",
                        .{ self.type_pool.format(right_type), self.type_pool.format(left_type) },
                        node.span,
                    );
                }
                break :blk left_type;
            },

            // Range produces a range type (simplified: just return the element type).
            .range, .range_inclusive => left_type,

            // is operator returns bool.
            .@"is" => BuiltinTypes.bool_type,
        };
    }

    /// Check a unary operation.
    fn checkUnaryOp(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.unary;
        const operand_type = try self.checkExpr(data.operand);
        const operand = self.type_pool.get(operand_type);

        if (operand == null) return invalid_type;

        return switch (data.op) {
            .neg => blk: {
                if (!operand.?.isNumeric()) {
                    try self.emitError("negation requires numeric operand", .{}, node.span);
                    break :blk invalid_type;
                }
                break :blk operand_type;
            },
            .not => blk: {
                if (operand_type != BuiltinTypes.bool_type) {
                    try self.emitError("'not' requires bool operand", .{}, node.span);
                    break :blk BuiltinTypes.bool_type;
                }
                break :blk BuiltinTypes.bool_type;
            },
            .bit_not => blk: {
                if (!operand.?.isInteger()) {
                    try self.emitError("bitwise not requires integer operand", .{}, node.span);
                    break :blk invalid_type;
                }
                break :blk operand_type;
            },
            .ref => try self.type_pool.makeReference(operand_type, false),
            .ref_mut => try self.type_pool.makeReference(operand_type, true),
            .deref => blk: {
                if (operand.?.kind != .reference) {
                    try self.emitError("dereference requires reference type", .{}, node.span);
                    break :blk invalid_type;
                }
                break :blk operand.?.kind.reference.pointee;
            },
            .try_op => blk: {
                if (operand.?.kind != .optional) {
                    try self.emitError("'?' requires optional type", .{}, node.span);
                    break :blk invalid_type;
                }
                break :blk operand.?.kind.optional.inner;
            },
            .await_op => operand_type, // Simplified: just return the type.
        };
    }

    /// Check field access.
    fn checkFieldAccess(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.field_access;
        const object_type = try self.checkExpr(data.object);
        const object = self.type_pool.get(object_type);

        if (object == null) return invalid_type;

        const field_name = self.tree.getString(data.field_name) orelse return invalid_type;

        // Check if object is a struct.
        if (object.?.kind == .@"struct") {
            for (object.?.kind.@"struct".fields) |field| {
                if (std.mem.eql(u8, field.name, field_name)) {
                    return field.type_id;
                }
            }
            try self.emitError("no field '{s}' on type {s}", .{ field_name, self.type_pool.format(object_type) }, node.span);
        } else if (object.?.kind == .tuple) {
            // Tuple field access like .0, .1, etc.
            const idx = std.fmt.parseInt(usize, field_name, 10) catch {
                try self.emitError("invalid tuple index '{s}'", .{field_name}, node.span);
                return invalid_type;
            };
            if (idx >= object.?.kind.tuple.elements.len) {
                try self.emitError("tuple index {d} out of bounds", .{idx}, node.span);
                return invalid_type;
            }
            return object.?.kind.tuple.elements[idx];
        } else {
            try self.emitError("cannot access field on type {s}", .{self.type_pool.format(object_type)}, node.span);
        }

        return invalid_type;
    }

    /// Check index access.
    fn checkIndexAccess(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.index;
        const object_type = try self.checkExpr(data.object);
        const index_type = try self.checkExpr(data.index_expr);

        const object = self.type_pool.get(object_type);
        if (object == null) return invalid_type;

        // Check index is an integer.
        const index_t = self.type_pool.get(index_type);
        if (index_t == null or !index_t.?.isInteger()) {
            try self.emitError("index must be an integer", .{}, node.span);
        }

        // Return element type.
        return switch (object.?.kind) {
            .array => |a| a.element,
            .slice => |s| s.element,
            else => blk: {
                try self.emitError("cannot index type {s}", .{self.type_pool.format(object_type)}, node.span);
                break :blk invalid_type;
            },
        };
    }

    /// Check a function call.
    fn checkCall(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.call;
        const callee_type = try self.checkExpr(data.callee);

        const callee = self.type_pool.get(callee_type);
        if (callee == null) return invalid_type;

        if (callee.?.kind != .function) {
            try self.emitError("cannot call non-function type {s}", .{self.type_pool.format(callee_type)}, node.span);
            return invalid_type;
        }

        const fn_type = callee.?.kind.function;

        // Check argument count.
        const arg_count = data.args.end - data.args.start;
        if (arg_count != fn_type.params.len) {
            try self.emitError(
                "expected {d} arguments, found {d}",
                .{ fn_type.params.len, arg_count },
                node.span,
            );
        }

        // Check argument types.
        var i: usize = 0;
        for (data.args.start..data.args.end) |arg_idx| {
            if (i >= fn_type.params.len) break;
            const arg_type = try self.checkExpr(@intCast(arg_idx));
            const param_type = fn_type.params[i];
            const compat = checkCompatibility(&self.type_pool, arg_type, param_type);
            if (compat == .incompatible) {
                try self.emitError(
                    "argument type mismatch: expected {s}, found {s}",
                    .{ self.type_pool.format(param_type), self.type_pool.format(arg_type) },
                    node.span,
                );
            }
            i += 1;
        }

        return fn_type.return_type;
    }

    /// Check a method call.
    fn checkMethodCall(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.method_call;
        const receiver_type = try self.checkExpr(data.receiver);

        // Check arguments.
        for (data.args.start..data.args.end) |arg_idx| {
            _ = try self.checkExpr(@intCast(arg_idx));
        }

        _ = receiver_type;
        // TODO: Look up method on receiver type.
        return invalid_type;
    }

    /// Check an if expression.
    fn checkIfExpr(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.if_expr;

        // Check condition is bool.
        const cond_type = try self.checkExpr(data.condition);
        if (cond_type != BuiltinTypes.bool_type) {
            try self.emitError(
                "if condition must be bool, found {s}",
                .{self.type_pool.format(cond_type)},
                node.span,
            );
        }

        // Check branches.
        const then_type = try self.checkExpr(data.then_block);

        if (data.else_block != null_node) {
            const else_type = try self.checkExpr(data.else_block);

            // Branches must have compatible types.
            const compat = checkCompatibility(&self.type_pool, then_type, else_type);
            if (compat == .incompatible) {
                const compat_rev = checkCompatibility(&self.type_pool, else_type, then_type);
                if (compat_rev == .incompatible) {
                    try self.emitError(
                        "if branches have incompatible types: {s} and {s}",
                        .{ self.type_pool.format(then_type), self.type_pool.format(else_type) },
                        node.span,
                    );
                }
                return else_type;
            }
            return then_type;
        }

        // No else branch: type is void or optional.
        return BuiltinTypes.void_type;
    }

    /// Check a match expression.
    fn checkMatchExpr(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.match_expr;

        // Check the matched value.
        _ = try self.checkExpr(data.value);

        // Check all arms and ensure consistent types.
        var result_type: TypeId = invalid_type;

        for (data.arms.start..data.arms.end) |arm_idx| {
            const arm = self.tree.getNode(@intCast(arm_idx)) orelse continue;
            if (arm.tag == .match_arm) {
                const arm_data = arm.data.match_arm;
                const arm_type = try self.checkExpr(arm_data.body);

                if (result_type == invalid_type) {
                    result_type = arm_type;
                } else {
                    const compat = checkCompatibility(&self.type_pool, arm_type, result_type);
                    if (compat == .incompatible) {
                        try self.emitError(
                            "match arm type {s} incompatible with {s}",
                            .{ self.type_pool.format(arm_type), self.type_pool.format(result_type) },
                            arm.span,
                        );
                    }
                }
            }
        }

        return result_type;
    }

    /// Check a block expression.
    fn checkBlock(self: *Checker, node: Node) CheckerError!TypeId {
        const range = node.data.range;

        try self.pushScope();
        defer self.popScope();

        var result_type: TypeId = BuiltinTypes.void_type;

        for (range.start..range.end) |i| {
            const child = self.tree.getNode(@intCast(i)) orelse continue;

            // Check if it's a statement or expression.
            switch (child.tag) {
                .let_stmt, .var_stmt, .const_stmt, .return_stmt, .while_stmt, .for_stmt, .loop_stmt, .break_stmt, .continue_stmt => {
                    try self.checkNode(@intCast(i));
                },
                .expr_stmt => {
                    try self.checkExprStmt(child);
                },
                else => {
                    // Last expression is the block's value.
                    result_type = try self.checkExpr(@intCast(i));
                },
            }
        }

        return result_type;
    }

    /// Check a grouped expression.
    fn checkGrouped(self: *Checker, node: Node) CheckerError!TypeId {
        return self.checkExpr(node.data.node);
    }

    /// Check an array literal.
    fn checkArrayLiteral(self: *Checker, node: Node) CheckerError!TypeId {
        const range = node.data.range;

        if (range.isEmpty()) {
            // Empty array - type cannot be inferred.
            try self.emitError("cannot infer type of empty array literal", .{}, node.span);
            return invalid_type;
        }

        // Check all elements have the same type.
        var element_type: TypeId = invalid_type;
        for (range.start..range.end) |i| {
            const elem_type = try self.checkExpr(@intCast(i));
            if (element_type == invalid_type) {
                element_type = elem_type;
            } else {
                const compat = checkCompatibility(&self.type_pool, elem_type, element_type);
                if (compat == .incompatible) {
                    try self.emitError(
                        "array element type mismatch: expected {s}, found {s}",
                        .{ self.type_pool.format(element_type), self.type_pool.format(elem_type) },
                        node.span,
                    );
                }
            }
        }

        const size = range.end - range.start;
        return self.type_pool.makeArray(element_type, size);
    }

    /// Check a struct literal.
    fn checkStructLiteral(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.struct_literal;

        // Resolve the struct type.
        const struct_type = try self.resolveTypeNode(data.type_name);
        const struct_def = self.type_pool.get(struct_type);

        if (struct_def == null or struct_def.?.kind != .@"struct") {
            try self.emitError("expected struct type", .{}, node.span);
            return invalid_type;
        }

        // Check field initializers.
        for (data.fields.start..data.fields.end) |i| {
            const field_init = self.tree.getNode(@intCast(i)) orelse continue;
            if (field_init.tag == .field_init) {
                const init_data = field_init.data.field_init;
                const field_value_type = try self.checkExpr(init_data.value);

                // Find the field in the struct definition.
                const field_name = self.tree.getString(init_data.name) orelse continue;
                var found = false;
                for (struct_def.?.kind.@"struct".fields) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        found = true;
                        const compat = checkCompatibility(&self.type_pool, field_value_type, field.type_id);
                        if (compat == .incompatible) {
                            try self.emitError(
                                "field '{s}' type mismatch: expected {s}, found {s}",
                                .{ field_name, self.type_pool.format(field.type_id), self.type_pool.format(field_value_type) },
                                field_init.span,
                            );
                        }
                        break;
                    }
                }
                if (!found) {
                    try self.emitError("unknown field '{s}'", .{field_name}, field_init.span);
                }
            }
        }

        return struct_type;
    }

    /// Check a lambda expression.
    fn checkLambda(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.lambda;

        // Collect parameter types.
        var param_types = std.ArrayListUnmanaged(TypeId){};
        defer param_types.deinit(self.allocator);

        try self.pushScope();
        defer self.popScope();

        for (data.params.start..data.params.end) |i| {
            const param = self.tree.getNode(@intCast(i)) orelse continue;
            if (param.tag == .param) {
                const param_data = param.data.param;
                const param_type = if (param_data.type_node != null_node)
                    try self.resolveTypeNode(param_data.type_node)
                else
                    invalid_type; // Type must be inferred from context

                try param_types.append(self.allocator, param_type);

                if (self.tree.getString(param_data.name)) |name| {
                    try self.defineSymbol(name, .{
                        .name = name,
                        .type_id = param_type,
                        .kind = .parameter,
                        .is_mutable = param_data.is_mut,
                        .span = param.span,
                    });
                }
            }
        }

        // Check body.
        const body_type = try self.checkExpr(data.body);

        return self.type_pool.makeFunction(param_types.items, body_type);
    }

    /// Check a range expression.
    fn checkRange(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.range_expr;

        var element_type: TypeId = invalid_type;

        if (data.start != null_node) {
            element_type = try self.checkExpr(data.start);
        }
        if (data.end != null_node) {
            const end_type = try self.checkExpr(data.end);
            if (element_type == invalid_type) {
                element_type = end_type;
            } else {
                const compat = checkCompatibility(&self.type_pool, end_type, element_type);
                if (compat == .incompatible) {
                    try self.emitError(
                        "range bounds have incompatible types",
                        .{},
                        node.span,
                    );
                }
            }
        }

        // Return a slice type (simplified range representation).
        if (element_type != invalid_type) {
            return self.type_pool.makeSlice(element_type);
        }
        return invalid_type;
    }

    // === Type Resolution ===

    /// Resolve a type node to a TypeId.
    pub fn resolveTypeNode(self: *Checker, index: NodeIndex) CheckerError!TypeId {
        const node = self.tree.getNode(index) orelse return invalid_type;

        return switch (node.tag) {
            .type_named => try self.resolveNamedType(node),
            .type_generic => try self.resolveGenericType(node),
            .type_function => try self.resolveFunctionType(node),
            .type_reference => try self.resolveReferenceType(node),
            .type_optional => try self.resolveOptionalType(node),
            .type_array => try self.resolveArrayType(node),
            .type_slice => try self.resolveSliceType(node),
            .type_tuple => try self.resolveTupleType(node),
            .self_type => self.self_type,
            else => invalid_type,
        };
    }

    fn resolveNamedType(self: *Checker, node: Node) CheckerError!TypeId {
        const name_idx = node.data.string;
        const name = self.tree.getString(name_idx) orelse return invalid_type;

        // Check for primitive types.
        if (Primitive.fromString(name)) |_| {
            if (self.type_pool.getByName(name)) |id| {
                return id;
            }
        }

        // Check symbol table.
        if (self.lookupSymbol(name)) |sym| {
            return sym.type_id;
        }

        try self.emitError("undefined type '{s}'", .{name}, node.span);
        return invalid_type;
    }

    fn resolveGenericType(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.generic_type;

        const base = try self.resolveTypeNode(data.base_type);

        var args = std.ArrayListUnmanaged(TypeId){};
        defer args.deinit(self.allocator);

        for (data.type_args.start..data.type_args.end) |i| {
            const arg = try self.resolveTypeNode(@intCast(i));
            try args.append(self.allocator, arg);
        }

        return self.type_pool.makeGeneric(base, args.items);
    }

    fn resolveFunctionType(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.fn_type;

        var params = std.ArrayListUnmanaged(TypeId){};
        defer params.deinit(self.allocator);

        for (data.param_types.start..data.param_types.end) |i| {
            const param = try self.resolveTypeNode(@intCast(i));
            try params.append(self.allocator, param);
        }

        const return_type = if (data.return_type != null_node)
            try self.resolveTypeNode(data.return_type)
        else
            BuiltinTypes.void_type;

        return self.type_pool.makeFunction(params.items, return_type);
    }

    fn resolveReferenceType(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.ref_type;
        const pointee = try self.resolveTypeNode(data.pointee);
        return self.type_pool.makeReference(pointee, data.is_mut);
    }

    fn resolveOptionalType(self: *Checker, node: Node) CheckerError!TypeId {
        const inner = try self.resolveTypeNode(node.data.node);
        return self.type_pool.makeOptional(inner);
    }

    fn resolveArrayType(self: *Checker, node: Node) CheckerError!TypeId {
        const data = node.data.array_type;
        const element = try self.resolveTypeNode(data.element_type);

        // TODO: Evaluate size expression at compile time.
        // For now, use a placeholder size.
        return self.type_pool.makeArray(element, 0);
    }

    fn resolveSliceType(self: *Checker, node: Node) CheckerError!TypeId {
        const element = try self.resolveTypeNode(node.data.node);
        return self.type_pool.makeSlice(element);
    }

    fn resolveTupleType(self: *Checker, node: Node) CheckerError!TypeId {
        const range = node.data.range;

        var elements = std.ArrayListUnmanaged(TypeId){};
        defer elements.deinit(self.allocator);

        for (range.start..range.end) |i| {
            const elem = try self.resolveTypeNode(@intCast(i));
            try elements.append(self.allocator, elem);
        }

        return self.type_pool.makeTuple(elements.items);
    }

    // === Helper Functions ===

    fn resolveFnType(self: *Checker, data: @TypeOf(@as(Node.Data, undefined).fn_decl)) CheckerError!TypeId {
        var params = std.ArrayListUnmanaged(TypeId){};
        defer params.deinit(self.allocator);

        for (data.params.start..data.params.end) |i| {
            const param = self.tree.getNode(@intCast(i)) orelse continue;
            if (param.tag == .param) {
                const param_type = try self.resolveTypeNode(param.data.param.type_node);
                try params.append(self.allocator, param_type);
            }
        }

        const return_type = if (data.return_type != null_node)
            try self.resolveTypeNode(data.return_type)
        else
            BuiltinTypes.void_type;

        return self.type_pool.makeFunction(params.items, return_type);
    }

    fn registerStructType(self: *Checker, _: NodeIndex, name: []const u8, data: @TypeOf(@as(Node.Data, undefined).struct_decl)) CheckerError!TypeId {
        var fields = std.ArrayListUnmanaged(types.Field){};
        defer fields.deinit(self.allocator);

        for (data.fields.start..data.fields.end) |i| {
            const field = self.tree.getNode(@intCast(i)) orelse continue;
            if (field.tag == .struct_field) {
                const field_data = field.data.field;
                const field_name = self.tree.getString(field_data.name) orelse continue;
                const field_type = try self.resolveTypeNode(field_data.type_node);

                const owned_name = try self.allocator.dupe(u8, field_name);
                try fields.append(self.allocator, .{
                    .name = owned_name,
                    .type_id = field_type,
                    .is_pub = field_data.is_pub,
                    .span = field.span,
                });
            }
        }

        var generic_params = std.ArrayListUnmanaged(types.GenericParam){};
        defer generic_params.deinit(self.allocator);
        // TODO: Process generic parameters.

        const owned_name = try self.allocator.dupe(u8, name);
        const owned_fields = try self.allocator.dupe(types.Field, fields.items);
        const owned_generics = try self.allocator.dupe(types.GenericParam, generic_params.items);

        return self.type_pool.addType(.{
            .kind = .{
                .@"struct" = .{
                    .name = owned_name,
                    .fields = owned_fields,
                    .generic_params = owned_generics,
                    .is_pub = data.is_pub,
                },
            },
            .span = null,
        });
    }

    fn registerEnumType(self: *Checker, _: NodeIndex, name: []const u8, data: @TypeOf(@as(Node.Data, undefined).enum_decl)) CheckerError!TypeId {
        var variants = std.ArrayListUnmanaged(types.Variant){};
        defer variants.deinit(self.allocator);

        for (data.variants.start..data.variants.end) |i| {
            const variant = self.tree.getNode(@intCast(i)) orelse continue;
            if (variant.tag == .enum_variant) {
                const variant_data = variant.data.variant;
                const variant_name = self.tree.getString(variant_data.name) orelse continue;

                const payload = if (variant_data.payload != null_node)
                    try self.resolveTypeNode(variant_data.payload)
                else
                    invalid_type;

                const owned_name = try self.allocator.dupe(u8, variant_name);
                try variants.append(self.allocator, .{
                    .name = owned_name,
                    .payload = payload,
                    .span = variant.span,
                });
            }
        }

        var generic_params = std.ArrayListUnmanaged(types.GenericParam){};
        defer generic_params.deinit(self.allocator);

        const owned_name = try self.allocator.dupe(u8, name);
        const owned_variants = try self.allocator.dupe(types.Variant, variants.items);
        const owned_generics = try self.allocator.dupe(types.GenericParam, generic_params.items);

        return self.type_pool.addType(.{
            .kind = .{
                .@"enum" = .{
                    .name = owned_name,
                    .variants = owned_variants,
                    .generic_params = owned_generics,
                    .is_pub = data.is_pub,
                },
            },
            .span = null,
        });
    }

    fn registerTraitType(self: *Checker, name: []const u8, data: @TypeOf(@as(Node.Data, undefined).trait_decl)) CheckerError!TypeId {
        var methods = std.ArrayListUnmanaged(types.Method){};
        defer methods.deinit(self.allocator);

        for (data.methods.start..data.methods.end) |i| {
            const method = self.tree.getNode(@intCast(i)) orelse continue;
            if (method.tag == .fn_decl) {
                const method_data = method.data.fn_decl;
                const method_name = self.tree.getString(method_data.name) orelse continue;

                var params = std.ArrayListUnmanaged(TypeId){};
                defer params.deinit(self.allocator);

                for (method_data.params.start..method_data.params.end) |j| {
                    const param = self.tree.getNode(@intCast(j)) orelse continue;
                    if (param.tag == .param) {
                        const param_type = try self.resolveTypeNode(param.data.param.type_node);
                        try params.append(self.allocator, param_type);
                    }
                }

                const return_type = if (method_data.return_type != null_node)
                    try self.resolveTypeNode(method_data.return_type)
                else
                    BuiltinTypes.void_type;

                const owned_name = try self.allocator.dupe(u8, method_name);
                const owned_params = try self.allocator.dupe(TypeId, params.items);
                try methods.append(self.allocator, .{
                    .name = owned_name,
                    .params = owned_params,
                    .return_type = return_type,
                    .has_default = method_data.body != null_node,
                    .span = method.span,
                });
            }
        }

        var generic_params = std.ArrayListUnmanaged(types.GenericParam){};
        defer generic_params.deinit(self.allocator);

        const owned_name = try self.allocator.dupe(u8, name);
        const owned_methods = try self.allocator.dupe(types.Method, methods.items);
        const owned_generics = try self.allocator.dupe(types.GenericParam, generic_params.items);

        return self.type_pool.addType(.{
            .kind = .{
                .trait = .{
                    .name = owned_name,
                    .methods = owned_methods,
                    .generic_params = owned_generics,
                    .is_pub = data.is_pub,
                },
            },
            .span = null,
        });
    }

    fn promoteNumeric(self: *Checker, left: TypeId, right: TypeId) TypeId {
        const left_t = self.type_pool.get(left) orelse return invalid_type;
        const right_t = self.type_pool.get(right) orelse return invalid_type;

        // Float takes precedence.
        if (left_t.isFloat()) return left;
        if (right_t.isFloat()) return right;

        // Larger integer type.
        const left_size = if (left_t.kind == .primitive) left_t.kind.primitive.bitSize() else null;
        const right_size = if (right_t.kind == .primitive) right_t.kind.primitive.bitSize() else null;

        if (left_size != null and right_size != null) {
            if (left_size.? >= right_size.?) return left;
            return right;
        }

        return left;
    }

    // === Scope Management ===

    fn pushScope(self: *Checker) CheckerError!void {
        var new_scope = Scope.init();
        if (self.scopes.items.len > 0) {
            new_scope.parent = &self.scopes.items[self.scopes.items.len - 1];
        }
        try self.scopes.append(self.allocator, new_scope);
    }

    fn popScope(self: *Checker) void {
        if (self.scopes.items.len > 0) {
            // Get mutable reference to last scope before removing
            self.scopes.items[self.scopes.items.len - 1].deinit(self.allocator);
            _ = self.scopes.pop();
        }
    }

    fn currentScope(self: *Checker) ?*Scope {
        if (self.scopes.items.len > 0) {
            return &self.scopes.items[self.scopes.items.len - 1];
        }
        return null;
    }

    fn defineSymbol(self: *Checker, name: []const u8, symbol: Symbol) CheckerError!void {
        if (self.currentScope()) |scope| {
            // Check for redefinition in current scope.
            if (scope.lookupLocal(name) != null) {
                try self.emitError("redefinition of '{s}'", .{name}, symbol.span);
                return;
            }
            try scope.define(self.allocator, name, symbol);
        }
    }

    fn lookupSymbol(self: *const Checker, name: []const u8) ?Symbol {
        if (self.scopes.items.len > 0) {
            return self.scopes.items[self.scopes.items.len - 1].lookup(name);
        }
        return null;
    }

    // === Diagnostics ===

    fn emitError(self: *Checker, comptime fmt: []const u8, args: anytype, span: Span) CheckerError!void {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, fmt, args) catch "error formatting message";
        const owned = try self.allocator.dupe(u8, msg);
        try self.diagnostics.add(.{
            .severity = .@"error",
            .message = owned,
            .span = span,
            .code = null,
            .hints = &.{},
        });
    }

    /// Get the diagnostics list.
    pub fn getDiagnostics(self: *const Checker) []const Diagnostic {
        return self.diagnostics.items.items;
    }

    /// Check if there are any errors.
    pub fn hasErrors(self: *const Checker) bool {
        return self.diagnostics.has_errors;
    }
};

// === Tests ===

test "Checker initialization" {
    const allocator = std.testing.allocator;
    var tree = Ast.init(allocator, "", 0);
    defer tree.deinit();

    var checker = try Checker.init(allocator, &tree);
    defer checker.deinit();

    try std.testing.expect(!checker.hasErrors());
}

test "Scope lookup" {
    const allocator = std.testing.allocator;

    var scope1 = Scope.init();
    defer scope1.deinit(allocator);

    try scope1.define(allocator, "x", .{
        .name = "x",
        .type_id = BuiltinTypes.i32_type,
        .kind = .variable,
        .is_mutable = false,
        .span = Span.empty,
    });

    // Lookup in current scope.
    const sym = scope1.lookup("x");
    try std.testing.expect(sym != null);
    try std.testing.expectEqual(BuiltinTypes.i32_type, sym.?.type_id);

    // Lookup non-existent.
    try std.testing.expect(scope1.lookup("y") == null);
}

test "Scope parent lookup" {
    const allocator = std.testing.allocator;

    var parent = Scope.init();
    defer parent.deinit(allocator);

    try parent.define(allocator, "x", .{
        .name = "x",
        .type_id = BuiltinTypes.i32_type,
        .kind = .variable,
        .is_mutable = false,
        .span = Span.empty,
    });

    var child = Scope.init();
    child.parent = &parent;
    defer child.deinit(allocator);

    try child.define(allocator, "y", .{
        .name = "y",
        .type_id = BuiltinTypes.bool_type,
        .kind = .variable,
        .is_mutable = false,
        .span = Span.empty,
    });

    // Lookup in child should find parent's symbol.
    const sym_x = child.lookup("x");
    try std.testing.expect(sym_x != null);
    try std.testing.expectEqual(BuiltinTypes.i32_type, sym_x.?.type_id);

    // Lookup child's own symbol.
    const sym_y = child.lookup("y");
    try std.testing.expect(sym_y != null);
    try std.testing.expectEqual(BuiltinTypes.bool_type, sym_y.?.type_id);
}

// === Integration Tests ===

const parser = @import("parser.zig");

fn parseAndCheck(allocator: Allocator, source: []const u8) !struct { checker_inst: Checker, tree: Ast } {
    var p = parser.Parser.init(allocator, source, 0);
    defer p.deinit();

    var tree = try p.parse();
    errdefer tree.deinit();

    var checker_inst = try Checker.init(allocator, &tree);
    try checker_inst.check();

    return .{ .checker_inst = checker_inst, .tree = tree };
}

test "Type check: let with type annotation" {
    const allocator = std.testing.allocator;
    const source = "let x: i32 = 42";

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: let with type inference" {
    const allocator = std.testing.allocator;
    const source = "let x = 42";

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: var mutable binding" {
    const allocator = std.testing.allocator;
    const source = "var x: i32 = 42";

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: binary arithmetic" {
    const allocator = std.testing.allocator;
    const source = "let x = 1 + 2";

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: binary comparison" {
    const allocator = std.testing.allocator;
    const source = "let x = 1 < 2";

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: boolean literal" {
    const allocator = std.testing.allocator;
    const source = "let x: bool = true";

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: string literal" {
    const allocator = std.testing.allocator;
    const source =
        \\let s: str = "hello"
    ;

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: function declaration" {
    const allocator = std.testing.allocator;
    const source =
        \\fn add(a: i32, b: i32) -> i32 {
        \\    a + b
        \\}
    ;

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: function with return statement" {
    const allocator = std.testing.allocator;
    const source =
        \\fn double(x: i32) -> i32 {
        \\    return x + x
        \\}
    ;

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: undefined identifier error" {
    const allocator = std.testing.allocator;
    const source = "let x = undefined_var";

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(result.checker_inst.hasErrors());
    try std.testing.expect(result.checker_inst.diagnostics.items.items.len >= 1);
}

test "Type check: type mismatch error" {
    const allocator = std.testing.allocator;
    const source = "let x: bool = 42";

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(result.checker_inst.hasErrors());
}

test "Type check: logical operation with non-bool error" {
    const allocator = std.testing.allocator;
    const source = "let x = 1 and 2";

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(result.checker_inst.hasErrors());
}

test "Type check: if expression" {
    const allocator = std.testing.allocator;
    const source =
        \\let x = if true { 1 } else { 2 }
    ;

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: if condition must be bool" {
    const allocator = std.testing.allocator;
    const source =
        \\let x = if 42 { 1 } else { 2 }
    ;

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(result.checker_inst.hasErrors());
}

test "Type check: while loop" {
    const allocator = std.testing.allocator;
    const source =
        \\while true {
        \\    let x = 1
        \\}
    ;

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: struct declaration" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Point {
        \\    x: i32,
        \\    y: i32
        \\}
    ;

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: enum declaration" {
    const allocator = std.testing.allocator;
    const source =
        \\enum Color {
        \\    Red,
        \\    Green,
        \\    Blue
        \\}
    ;

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: unary negation" {
    const allocator = std.testing.allocator;
    const source = "let x = -42";

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: unary not" {
    const allocator = std.testing.allocator;
    const source = "let x = not true";

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: reference type" {
    const allocator = std.testing.allocator;
    const source =
        \\fn foo(x: &i32) -> i32 {
        \\    *x
        \\}
    ;

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}

test "Type check: nested scopes" {
    const allocator = std.testing.allocator;
    const source =
        \\fn foo() -> i32 {
        \\    let x = 1
        \\    {
        \\        let y = 2
        \\        x + y
        \\    }
        \\}
    ;

    var result = try parseAndCheck(allocator, source);
    defer result.checker_inst.deinit();
    defer result.tree.deinit();

    try std.testing.expect(!result.checker_inst.hasErrors());
}
