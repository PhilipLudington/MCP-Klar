//! Abstract Syntax Tree (AST) node types for the Klar language.
//!
//! The AST represents the syntactic structure of Klar source code after parsing.
//! All nodes include span information for error reporting and source mapping.

const std = @import("std");
const Allocator = std.mem.Allocator;
const utils = @import("utils");

const Span = utils.Span;

/// Index into an AST node array. Used for child references.
pub const NodeIndex = u32;

/// Sentinel value indicating no node.
pub const null_node: NodeIndex = std.math.maxInt(NodeIndex);

/// A range of node indices [start, end).
pub const NodeRange = struct {
    start: NodeIndex,
    end: NodeIndex,

    pub const empty = NodeRange{ .start = 0, .end = 0 };

    pub fn len(self: NodeRange) usize {
        return self.end - self.start;
    }

    pub fn isEmpty(self: NodeRange) bool {
        return self.start == self.end;
    }
};

/// String index into the string table.
pub const StringIndex = u32;

/// Sentinel value for no string.
pub const null_string: StringIndex = std.math.maxInt(StringIndex);

/// AST node representing all syntactic constructs in Klar.
pub const Node = struct {
    tag: Tag,
    span: Span,
    data: Data,

    /// Node tags identify what kind of syntactic construct a node represents.
    pub const Tag = enum {
        // === Expressions ===

        /// Integer literal: `42`, `0xFF`, `1_000`
        integer_literal,
        /// Float literal: `3.14`, `1e10`
        float_literal,
        /// String literal: `"hello"`
        string_literal,
        /// Character literal: `'a'`
        char_literal,
        /// Boolean literal: `true`, `false`
        bool_literal,
        /// Identifier: `foo`, `my_var`
        identifier,
        /// Self reference: `self`
        self_expr,
        /// Self type: `Self`
        self_type,

        /// Binary operation: `a + b`, `x == y`
        binary_op,
        /// Unary operation: `-x`, `not y`, `!z`
        unary_op,

        /// Function call: `foo(a, b)`
        call,
        /// Method call: `obj.method(a, b)`
        method_call,
        /// Field access: `obj.field`
        field_access,
        /// Index access: `arr[i]`
        index_access,

        /// Block expression: `{ stmt; stmt; expr }`
        block,
        /// Grouped expression: `(expr)`
        grouped,

        /// If expression: `if cond { ... } else { ... }`
        if_expr,
        /// Match expression: `match val { pat => expr, ... }`
        match_expr,
        /// Match arm: `pattern => expression`
        match_arm,

        /// Range expression: `a..b` or `a..=b`
        range,

        /// Array literal: `[1, 2, 3]`
        array_literal,
        /// Struct literal: `Point { x: 1, y: 2 }`
        struct_literal,
        /// Field initializer: `name: value`
        field_init,

        /// Lambda/closure: `|x, y| x + y`
        lambda,

        // === Statements ===

        /// Let binding (immutable): `let x = expr` or `let x: T = expr`
        let_stmt,
        /// Var binding (mutable): `var x = expr` or `var x: T = expr`
        var_stmt,
        /// Const binding: `const X = expr`
        const_stmt,
        /// Return statement: `return expr`
        return_stmt,
        /// Break statement: `break` or `break expr`
        break_stmt,
        /// Continue statement: `continue`
        continue_stmt,
        /// While loop: `while cond { ... }`
        while_stmt,
        /// For loop: `for x in iter { ... }`
        for_stmt,
        /// Infinite loop: `loop { ... }`
        loop_stmt,
        /// Expression statement: `expr;`
        expr_stmt,

        // === Declarations ===

        /// Function declaration: `fn name(params) -> T { ... }`
        fn_decl,
        /// Function parameter: `name: Type`
        param,
        /// Struct declaration: `struct Name { ... }`
        struct_decl,
        /// Struct field: `name: Type`
        struct_field,
        /// Enum declaration: `enum Name { ... }`
        enum_decl,
        /// Enum variant: `Name` or `Name(T)` or `Name { ... }`
        enum_variant,
        /// Trait declaration: `trait Name { ... }`
        trait_decl,
        /// Impl block: `impl Trait for Type { ... }`
        impl_decl,
        /// Type alias: `type Name = Type`
        type_alias,
        /// Associated type declaration in trait: `type Item` or `type Item: Bound`
        associated_type,

        // === Type Annotations ===

        /// Named type: `i32`, `String`, `MyStruct`
        type_named,
        /// Generic type: `List[T]`, `Result[T, E]`
        type_generic,
        /// Function type: `fn(i32) -> bool`
        type_function,
        /// Reference type: `&T`, `&mut T`
        type_reference,
        /// Optional type: `?T`
        type_optional,
        /// Array type: `[T; N]`
        type_array,
        /// Slice type: `[T]`
        type_slice,
        /// Tuple type: `(T, U, V)`
        type_tuple,
        /// Qualified type: `Self.Item`, `T.Item`
        type_qualified,

        // === Module Level ===

        /// Import statement: `import std.io`
        import_stmt,
        /// Import item: `foo as bar`
        import_item,
        /// Module/file root
        module,

        // === Patterns (for match and let) ===

        /// Wildcard pattern: `_`
        pattern_wildcard,
        /// Identifier pattern: `x`
        pattern_ident,
        /// Literal pattern: `42`, `"hello"`
        pattern_literal,
        /// Tuple pattern: `(a, b, c)`
        pattern_tuple,
        /// Struct pattern: `Point { x, y }`
        pattern_struct,
        /// Enum variant pattern: `Some(x)`, `None`
        pattern_enum,

        // === Misc ===

        /// Generic parameter: `T` or `T: Trait`
        generic_param,
        /// Where clause constraint: `T: Clone`
        where_constraint,
        /// Visibility modifier: `pub`
        visibility,
        /// Doc comment attached to a declaration
        doc_comment,
        /// Error placeholder for recovery
        @"error",
    };

    /// Data payload varies by tag. Uses indices to reference other nodes/strings.
    pub const Data = union {
        /// No additional data
        none: void,

        /// Single node reference (e.g., unary operand, return value)
        node: NodeIndex,

        /// Two node references (e.g., binary op operands, if condition/then)
        two_nodes: struct {
            left: NodeIndex,
            right: NodeIndex,
        },

        /// Three node references (e.g., if with else)
        three_nodes: struct {
            first: NodeIndex,
            second: NodeIndex,
            third: NodeIndex,
        },

        /// String data (for identifiers, literals)
        string: StringIndex,

        /// Node with string (e.g., named type, identifier with binding)
        node_and_string: struct {
            node: NodeIndex,
            string: StringIndex,
        },

        /// Range of child nodes (for lists: params, fields, statements)
        range: NodeRange,

        /// Binary operator info
        binary: struct {
            op: BinaryOp,
            left: NodeIndex,
            right: NodeIndex,
        },

        /// Unary operator info
        unary: struct {
            op: UnaryOp,
            operand: NodeIndex,
        },

        /// Let/var statement data (used by let_stmt and var_stmt)
        let: struct {
            name: StringIndex,
            type_node: NodeIndex, // null_node if inferred
            value: NodeIndex,
        },

        /// Function declaration data
        fn_decl: struct {
            name: StringIndex,
            generics: NodeRange, // generic parameters
            params: NodeRange, // function parameters
            return_type: NodeIndex, // null_node if void
            where_clause: NodeRange, // where T: Clone constraints
            body: NodeIndex, // null_node for trait method signatures
            is_pub: bool,
        },

        /// Struct declaration data
        struct_decl: struct {
            name: StringIndex,
            generics: NodeRange,
            traits: NodeRange, // struct Point: Clone + Eq
            fields: NodeRange,
            is_pub: bool,
        },

        /// Enum declaration data
        enum_decl: struct {
            name: StringIndex,
            generics: NodeRange,
            variants: NodeRange,
            is_pub: bool,
        },

        /// Trait declaration data
        trait_decl: struct {
            name: StringIndex,
            generics: NodeRange,
            super_traits: NodeRange, // Trait bounds: trait Ord: Eq + Clone
            methods: NodeRange,
            is_pub: bool,
        },

        /// Impl block data
        impl_decl: struct {
            trait_type: NodeIndex, // null_node for inherent impl
            target_type: NodeIndex,
            generics: NodeRange,
            where_clause: NodeRange, // where T: Clone constraints
            methods: NodeRange,
        },

        /// Call expression data
        call: struct {
            callee: NodeIndex,
            args: NodeRange,
        },

        /// Method call data
        method_call: struct {
            receiver: NodeIndex,
            method_name: StringIndex,
            args: NodeRange,
        },

        /// Field access data
        field_access: struct {
            object: NodeIndex,
            field_name: StringIndex,
        },

        /// Index access data
        index: struct {
            object: NodeIndex,
            index_expr: NodeIndex,
        },

        /// If expression data
        if_expr: struct {
            condition: NodeIndex,
            then_block: NodeIndex,
            else_block: NodeIndex, // null_node if no else
        },

        /// Match expression data
        match_expr: struct {
            value: NodeIndex,
            arms: NodeRange,
        },

        /// Match arm data
        match_arm: struct {
            pattern: NodeIndex,
            guard: NodeIndex, // null_node if no guard
            body: NodeIndex,
        },

        /// For loop data
        for_loop: struct {
            binding: StringIndex,
            iterator: NodeIndex,
            body: NodeIndex,
        },

        /// While loop data
        while_loop: struct {
            condition: NodeIndex,
            body: NodeIndex,
        },

        /// Loop (infinite) data - body only, uses `node` field
        /// (Alternatively, loop just uses the `node` Data field directly)

        /// Parameter data
        param: struct {
            name: StringIndex,
            type_node: NodeIndex,
            is_mut: bool,
        },

        /// Struct field data
        field: struct {
            name: StringIndex,
            type_node: NodeIndex,
            is_pub: bool,
        },

        /// Enum variant data
        variant: struct {
            name: StringIndex,
            payload: NodeIndex, // type or struct fields, null_node if unit
        },

        /// Type alias data
        type_alias: struct {
            name: StringIndex,
            generics: NodeRange,
            aliased_type: NodeIndex,
            is_pub: bool,
        },

        /// Associated type data: `type Item` or `type Item: Bound` or `type Item = ConcreteType`
        associated_type: struct {
            name: StringIndex,
            bounds: NodeRange, // Trait bounds if in trait definition
            value: NodeIndex, // Concrete type if in impl block, null_node if declaration only
        },

        /// Generic type application: `List[T]`
        generic_type: struct {
            base_type: NodeIndex,
            type_args: NodeRange,
        },

        /// Reference type data
        ref_type: struct {
            pointee: NodeIndex,
            is_mut: bool,
        },

        /// Array type data: `[T; N]`
        array_type: struct {
            element_type: NodeIndex,
            size_expr: NodeIndex,
        },

        /// Function type data
        fn_type: struct {
            param_types: NodeRange,
            return_type: NodeIndex,
        },

        /// Qualified type data: `Self.Item`, `T.Item`
        qualified_type: struct {
            base_type: NodeIndex, // Self or T
            member_name: StringIndex, // Item
        },

        /// Range expression data
        range_expr: struct {
            start: NodeIndex, // null_node for `..end`
            end: NodeIndex, // null_node for `start..`
            is_inclusive: bool,
        },

        /// Lambda data
        lambda: struct {
            params: NodeRange,
            body: NodeIndex,
        },

        /// Import data
        import: struct {
            path: NodeRange, // list of identifier nodes forming the path
            items: NodeRange, // imported items, empty for `import mod`
            alias: StringIndex, // null_string if no alias
        },

        /// Generic parameter data
        generic_param: struct {
            name: StringIndex,
            bounds: NodeRange, // trait bounds
        },

        /// Struct literal field initialization
        field_init: struct {
            name: StringIndex,
            value: NodeIndex,
        },

        /// Struct literal data
        struct_literal: struct {
            type_name: NodeIndex,
            fields: NodeRange,
        },

        /// Bool literal value
        bool_value: bool,

        /// Convert to concrete type for reading (fails if wrong variant)
        pub fn asNone(self: Data) void {
            return self.none;
        }

        /// Read as single node index
        pub fn asNode(self: Data) NodeIndex {
            return self.node;
        }

        /// Read as string index
        pub fn asString(self: Data) StringIndex {
            return self.string;
        }
    };
};

/// Binary operators.
pub const BinaryOp = enum {
    // Arithmetic
    add, // +
    sub, // -
    mul, // *
    div, // /
    mod, // %

    // Wrapping arithmetic (overflow wraps)
    add_wrap, // +%
    sub_wrap, // -%
    mul_wrap, // *%

    // Saturating arithmetic (clamps at bounds)
    add_sat, // +|
    sub_sat, // -|
    mul_sat, // *|

    // Comparison
    eq, // ==
    ne, // !=
    lt, // <
    gt, // >
    le, // <=
    ge, // >=

    // Logical
    @"and", // and
    @"or", // or

    // Bitwise
    bit_and, // &
    bit_or, // |
    bit_xor, // ^
    shl, // <<
    shr, // >>

    // Null coalescing
    null_coalesce, // ??

    // Type checking
    @"is", // is

    // Assignment
    assign, // =
    add_assign, // +=
    sub_assign, // -=
    mul_assign, // *=
    div_assign, // /=
    mod_assign, // %=

    // Range
    range, // ..
    range_inclusive, // ..=

    /// Returns precedence level (higher = binds tighter).
    /// Based on Klar language specification.
    pub fn precedence(self: BinaryOp) u8 {
        return switch (self) {
            // Assignment (lowest)
            .assign, .add_assign, .sub_assign, .mul_assign, .div_assign, .mod_assign => 1,
            // Logical or
            .@"or" => 2,
            // Logical and
            .@"and" => 3,
            // Comparison and is
            .eq, .ne, .lt, .gt, .le, .ge, .@"is" => 4,
            // Null coalescing
            .null_coalesce => 5,
            // Bitwise or
            .bit_or => 6,
            // Bitwise xor
            .bit_xor => 7,
            // Bitwise and
            .bit_and => 8,
            // Bit shifts
            .shl, .shr => 9,
            // Range
            .range, .range_inclusive => 10,
            // Addition/subtraction (including wrapping/saturating)
            .add, .sub, .add_wrap, .sub_wrap, .add_sat, .sub_sat => 11,
            // Multiplication/division (including wrapping/saturating)
            .mul, .div, .mod, .mul_wrap, .mul_sat => 12,
        };
    }

    /// Returns true if operator is right-associative.
    pub fn isRightAssociative(self: BinaryOp) bool {
        return switch (self) {
            .assign, .add_assign, .sub_assign, .mul_assign, .div_assign, .mod_assign => true,
            else => false,
        };
    }
};

/// Unary operators.
pub const UnaryOp = enum {
    neg, // -
    not, // not
    bit_not, // ~
    ref, // &
    ref_mut, // &mut
    deref, // *
    try_op, // ? (postfix)
    await_op, // await (prefix)
};

/// The complete AST for a file, using arena allocation.
pub const Ast = struct {
    /// All nodes in the tree, stored contiguously.
    nodes: std.ArrayListUnmanaged(Node),

    /// String table for identifiers and literals.
    strings: std.ArrayListUnmanaged([]const u8),

    /// Source text (for extracting token text).
    source: []const u8,

    /// File identifier for span information.
    file_id: u32,

    /// Root node index (typically a module node).
    root: NodeIndex,

    /// Allocator used for the AST.
    allocator: Allocator,

    /// Diagnostics generated during parsing.
    errors: std.ArrayListUnmanaged(ParseError),

    pub const ParseError = struct {
        message: []const u8,
        span: Span,
    };

    pub fn init(allocator: Allocator, source: []const u8, file_id: u32) Ast {
        return .{
            .nodes = .{},
            .strings = .{},
            .source = source,
            .file_id = file_id,
            .root = null_node,
            .allocator = allocator,
            .errors = .{},
        };
    }

    pub fn deinit(self: *Ast) void {
        self.nodes.deinit(self.allocator);
        for (self.strings.items) |s| {
            self.allocator.free(s);
        }
        self.strings.deinit(self.allocator);
        for (self.errors.items) |e| {
            self.allocator.free(e.message);
        }
        self.errors.deinit(self.allocator);
    }

    /// Add a node to the AST, returning its index.
    pub fn addNode(self: *Ast, node: Node) !NodeIndex {
        const index: NodeIndex = @intCast(self.nodes.items.len);
        try self.nodes.append(self.allocator, node);
        return index;
    }

    /// Add a string to the string table, returning its index.
    pub fn addString(self: *Ast, str: []const u8) !StringIndex {
        const index: StringIndex = @intCast(self.strings.items.len);
        const owned = try self.allocator.dupe(u8, str);
        try self.strings.append(self.allocator, owned);
        return index;
    }

    /// Get a node by index.
    pub fn getNode(self: *const Ast, index: NodeIndex) ?Node {
        if (index == null_node or index >= self.nodes.items.len) {
            return null;
        }
        return self.nodes.items[index];
    }

    /// Get a string by index.
    pub fn getString(self: *const Ast, index: StringIndex) ?[]const u8 {
        if (index == null_string or index >= self.strings.items.len) {
            return null;
        }
        return self.strings.items[index];
    }

    /// Get nodes in a range.
    pub fn getNodeRange(self: *const Ast, range: NodeRange) []const Node {
        if (range.start >= self.nodes.items.len) {
            return &.{};
        }
        const end = @min(range.end, @as(NodeIndex, @intCast(self.nodes.items.len)));
        return self.nodes.items[range.start..end];
    }

    /// Add a parse error.
    pub fn addError(self: *Ast, message: []const u8, span: Span) !void {
        const owned_msg = try self.allocator.dupe(u8, message);
        try self.errors.append(self.allocator, .{
            .message = owned_msg,
            .span = span,
        });
    }

    /// Returns true if there are parse errors.
    pub fn hasErrors(self: *const Ast) bool {
        return self.errors.items.len > 0;
    }
};

// === Tests ===

test "NodeRange operations" {
    const range = NodeRange{ .start = 5, .end = 10 };
    try std.testing.expectEqual(@as(usize, 5), range.len());
    try std.testing.expect(!range.isEmpty());

    const empty_range = NodeRange.empty;
    try std.testing.expectEqual(@as(usize, 0), empty_range.len());
    try std.testing.expect(empty_range.isEmpty());
}

test "BinaryOp precedence" {
    // Multiplication binds tighter than addition
    try std.testing.expect(BinaryOp.mul.precedence() > BinaryOp.add.precedence());
    // Addition binds tighter than comparison
    try std.testing.expect(BinaryOp.add.precedence() > BinaryOp.eq.precedence());
    // Comparison binds tighter than logical and
    try std.testing.expect(BinaryOp.eq.precedence() > BinaryOp.@"and".precedence());
    // Logical and binds tighter than logical or
    try std.testing.expect(BinaryOp.@"and".precedence() > BinaryOp.@"or".precedence());
    // Assignment is lowest precedence
    try std.testing.expect(BinaryOp.assign.precedence() < BinaryOp.@"or".precedence());
}

test "BinaryOp associativity" {
    try std.testing.expect(BinaryOp.assign.isRightAssociative());
    try std.testing.expect(BinaryOp.add_assign.isRightAssociative());
    try std.testing.expect(!BinaryOp.add.isRightAssociative());
    try std.testing.expect(!BinaryOp.mul.isRightAssociative());
}

test "Ast basic operations" {
    const allocator = std.testing.allocator;
    var ast = Ast.init(allocator, "test source", 0);
    defer ast.deinit();

    // Add a string
    const str_idx = try ast.addString("hello");
    try std.testing.expectEqualStrings("hello", ast.getString(str_idx).?);

    // Add a node
    const node_idx = try ast.addNode(.{
        .tag = .identifier,
        .span = Span.empty,
        .data = .{ .string = str_idx },
    });
    const node = ast.getNode(node_idx).?;
    try std.testing.expectEqual(Node.Tag.identifier, node.tag);

    // Check null handling
    try std.testing.expect(ast.getNode(null_node) == null);
    try std.testing.expect(ast.getString(null_string) == null);
}

test "Ast error tracking" {
    const allocator = std.testing.allocator;
    var ast = Ast.init(allocator, "test", 0);
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());

    try ast.addError("unexpected token", Span.empty);
    try std.testing.expect(ast.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), ast.errors.items.len);
    try std.testing.expectEqualStrings("unexpected token", ast.errors.items[0].message);
}
