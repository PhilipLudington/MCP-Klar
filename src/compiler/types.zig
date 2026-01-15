//! Type representation for the Klar language type system.
//!
//! This module defines the runtime representation of types used during
//! type checking. Types are interned and represented by TypeId indices
//! into a TypePool.

const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const utils = @import("utils");

const NodeIndex = ast.NodeIndex;
const StringIndex = ast.StringIndex;
const Span = utils.Span;

/// Index into the type pool. Used for efficient type comparison and storage.
pub const TypeId = u32;

/// Sentinel value indicating an invalid or unknown type.
pub const invalid_type: TypeId = std.math.maxInt(TypeId);

/// Built-in type IDs (reserved indices).
pub const BuiltinTypes = struct {
    pub const void_type: TypeId = 0;
    pub const bool_type: TypeId = 1;
    pub const i8_type: TypeId = 2;
    pub const i16_type: TypeId = 3;
    pub const i32_type: TypeId = 4;
    pub const i64_type: TypeId = 5;
    pub const i128_type: TypeId = 6;
    pub const isize_type: TypeId = 7;
    pub const u8_type: TypeId = 8;
    pub const u16_type: TypeId = 9;
    pub const u32_type: TypeId = 10;
    pub const u64_type: TypeId = 11;
    pub const u128_type: TypeId = 12;
    pub const usize_type: TypeId = 13;
    pub const f32_type: TypeId = 14;
    pub const f64_type: TypeId = 15;
    pub const char_type: TypeId = 16;
    pub const str_type: TypeId = 17; // string slice type
    pub const never_type: TypeId = 18; // ! (never returns)

    /// First ID available for user-defined types.
    pub const first_user_type: TypeId = 19;
};

/// Represents a type in the Klar language.
pub const Type = struct {
    kind: Kind,
    /// Source span where this type was defined (null for built-ins).
    span: ?Span,

    pub const Kind = union(enum) {
        /// Primitive types: void, bool, integers, floats, char, str, never
        primitive: Primitive,

        /// Named type reference (struct, enum, trait, type alias).
        /// `name` is the fully qualified name.
        named: struct {
            name: []const u8,
            /// The definition site (TypeId of the actual definition).
            definition: TypeId,
        },

        /// Generic type instantiation: List[i32], Result[T, E]
        generic: struct {
            base: TypeId,
            args: []const TypeId,
        },

        /// Reference type: &T, &mut T
        reference: struct {
            pointee: TypeId,
            is_mut: bool,
        },

        /// Optional type: ?T
        optional: struct {
            inner: TypeId,
        },

        /// Array type: [T; N]
        array: struct {
            element: TypeId,
            size: usize,
        },

        /// Slice type: [T]
        slice: struct {
            element: TypeId,
        },

        /// Tuple type: (T, U, V)
        tuple: struct {
            elements: []const TypeId,
        },

        /// Function type: fn(T, U) -> V
        function: struct {
            params: []const TypeId,
            return_type: TypeId,
        },

        /// Struct type definition.
        @"struct": struct {
            name: []const u8,
            fields: []const Field,
            generic_params: []const GenericParam,
            is_pub: bool,
        },

        /// Enum type definition.
        @"enum": struct {
            name: []const u8,
            variants: []const Variant,
            generic_params: []const GenericParam,
            is_pub: bool,
        },

        /// Trait type definition.
        trait: struct {
            name: []const u8,
            methods: []const Method,
            generic_params: []const GenericParam,
            is_pub: bool,
        },

        /// Type alias.
        alias: struct {
            name: []const u8,
            target: TypeId,
            generic_params: []const GenericParam,
            is_pub: bool,
        },

        /// Type variable in a generic context: T, U, E
        type_var: struct {
            name: []const u8,
            /// Index in the generic parameter list.
            index: usize,
            /// Trait bounds.
            bounds: []const TypeId,
        },

        /// Error/unknown type for error recovery.
        @"error": void,
    };

    /// Check if this type is a primitive.
    pub fn isPrimitive(self: Type) bool {
        return self.kind == .primitive;
    }

    /// Check if this type is numeric (integer or float).
    pub fn isNumeric(self: Type) bool {
        return switch (self.kind) {
            .primitive => |p| p.isNumeric(),
            else => false,
        };
    }

    /// Check if this type is an integer.
    pub fn isInteger(self: Type) bool {
        return switch (self.kind) {
            .primitive => |p| p.isInteger(),
            else => false,
        };
    }

    /// Check if this type is a float.
    pub fn isFloat(self: Type) bool {
        return switch (self.kind) {
            .primitive => |p| p.isFloat(),
            else => false,
        };
    }

    /// Check if this is the void type.
    pub fn isVoid(self: Type) bool {
        return switch (self.kind) {
            .primitive => |p| p == .void,
            else => false,
        };
    }

    /// Check if this is the never type (!).
    pub fn isNever(self: Type) bool {
        return switch (self.kind) {
            .primitive => |p| p == .never,
            else => false,
        };
    }

    /// Check if this is the error type.
    pub fn isError(self: Type) bool {
        return self.kind == .@"error";
    }
};

/// Primitive types in Klar.
pub const Primitive = enum {
    void,
    bool,
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    u8,
    u16,
    u32,
    u64,
    u128,
    usize,
    f32,
    f64,
    char,
    str,
    never,

    /// Returns true if this is a numeric type (integer or float).
    pub fn isNumeric(self: Primitive) bool {
        return self.isInteger() or self.isFloat();
    }

    /// Returns true if this is an integer type.
    pub fn isInteger(self: Primitive) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .i128, .isize, .u8, .u16, .u32, .u64, .u128, .usize => true,
            else => false,
        };
    }

    /// Returns true if this is a signed integer type.
    pub fn isSigned(self: Primitive) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .i128, .isize => true,
            else => false,
        };
    }

    /// Returns true if this is an unsigned integer type.
    pub fn isUnsigned(self: Primitive) bool {
        return switch (self) {
            .u8, .u16, .u32, .u64, .u128, .usize => true,
            else => false,
        };
    }

    /// Returns true if this is a float type.
    pub fn isFloat(self: Primitive) bool {
        return switch (self) {
            .f32, .f64 => true,
            else => false,
        };
    }

    /// Returns the bit size of the type (0 for void/never, null for isize/usize).
    pub fn bitSize(self: Primitive) ?u16 {
        return switch (self) {
            .void, .never => 0,
            .bool => 1,
            .i8, .u8 => 8,
            .i16, .u16 => 16,
            .i32, .u32, .f32, .char => 32,
            .i64, .u64, .f64 => 64,
            .i128, .u128 => 128,
            .isize, .usize => null, // platform-dependent
            .str => null, // fat pointer
        };
    }

    /// Convert to string representation.
    pub fn toString(self: Primitive) []const u8 {
        return switch (self) {
            .void => "void",
            .bool => "bool",
            .i8 => "i8",
            .i16 => "i16",
            .i32 => "i32",
            .i64 => "i64",
            .i128 => "i128",
            .isize => "isize",
            .u8 => "u8",
            .u16 => "u16",
            .u32 => "u32",
            .u64 => "u64",
            .u128 => "u128",
            .usize => "usize",
            .f32 => "f32",
            .f64 => "f64",
            .char => "char",
            .str => "str",
            .never => "!",
        };
    }

    /// Parse a primitive type name.
    pub fn fromString(name: []const u8) ?Primitive {
        const map = std.StaticStringMap(Primitive).initComptime(.{
            .{ "void", .void },
            .{ "bool", .bool },
            .{ "i8", .i8 },
            .{ "i16", .i16 },
            .{ "i32", .i32 },
            .{ "i64", .i64 },
            .{ "i128", .i128 },
            .{ "isize", .isize },
            .{ "u8", .u8 },
            .{ "u16", .u16 },
            .{ "u32", .u32 },
            .{ "u64", .u64 },
            .{ "u128", .u128 },
            .{ "usize", .usize },
            .{ "f32", .f32 },
            .{ "f64", .f64 },
            .{ "char", .char },
            .{ "str", .str },
            .{ "never", .never },
        });
        return map.get(name);
    }
};

/// A struct field.
pub const Field = struct {
    name: []const u8,
    type_id: TypeId,
    is_pub: bool,
    span: ?Span,
};

/// An enum variant.
pub const Variant = struct {
    name: []const u8,
    /// Payload type (invalid_type if unit variant).
    payload: TypeId,
    span: ?Span,
};

/// A trait method signature.
pub const Method = struct {
    name: []const u8,
    params: []const TypeId,
    return_type: TypeId,
    /// True if this method has a default implementation.
    has_default: bool,
    span: ?Span,
};

/// A generic type parameter with optional trait bounds.
pub const GenericParam = struct {
    name: []const u8,
    bounds: []const TypeId,
    span: ?Span,
};

/// Pool of interned types.
/// Provides efficient type comparison (by TypeId) and prevents duplicate allocations.
pub const TypePool = struct {
    allocator: Allocator,
    types: std.ArrayListUnmanaged(Type),

    /// Maps type names to their definitions for quick lookup.
    name_map: std.StringHashMapUnmanaged(TypeId),

    pub fn init(allocator: Allocator) !TypePool {
        var pool = TypePool{
            .allocator = allocator,
            .types = .{},
            .name_map = .{},
        };

        // Pre-populate with built-in types.
        try pool.initBuiltins();

        return pool;
    }

    pub fn deinit(self: *TypePool) void {
        // Free allocated type data.
        for (self.types.items) |t| {
            switch (t.kind) {
                .generic => |g| self.allocator.free(g.args),
                .tuple => |tup| self.allocator.free(tup.elements),
                .function => |f| self.allocator.free(f.params),
                .@"struct" => |s| {
                    for (s.fields) |field| {
                        self.allocator.free(field.name);
                    }
                    self.allocator.free(s.fields);
                    self.allocator.free(s.generic_params);
                    self.allocator.free(s.name);
                },
                .@"enum" => |e| {
                    for (e.variants) |variant| {
                        self.allocator.free(variant.name);
                    }
                    self.allocator.free(e.variants);
                    self.allocator.free(e.generic_params);
                    self.allocator.free(e.name);
                },
                .trait => |tr| {
                    for (tr.methods) |m| {
                        self.allocator.free(m.name);
                        self.allocator.free(m.params);
                    }
                    self.allocator.free(tr.methods);
                    self.allocator.free(tr.generic_params);
                    self.allocator.free(tr.name);
                },
                .alias => |a| {
                    self.allocator.free(a.generic_params);
                    self.allocator.free(a.name);
                },
                .type_var => |tv| self.allocator.free(tv.bounds),
                .named => |n| self.allocator.free(n.name),
                else => {},
            }
        }
        self.types.deinit(self.allocator);
        self.name_map.deinit(self.allocator);
    }

    fn initBuiltins(self: *TypePool) !void {
        // Add built-in primitive types in order matching BuiltinTypes constants.
        const primitives = [_]Primitive{
            .void, .bool, .i8,    .i16,   .i32,   .i64,  .i128, .isize,
            .u8,   .u16,  .u32,   .u64,   .u128,  .usize, .f32, .f64,
            .char, .str,  .never,
        };

        for (primitives) |prim| {
            const id = try self.addType(.{
                .kind = .{ .primitive = prim },
                .span = null,
            });
            try self.name_map.put(self.allocator, prim.toString(), id);
        }
    }

    /// Add a new type to the pool.
    pub fn addType(self: *TypePool, t: Type) !TypeId {
        const id: TypeId = @intCast(self.types.items.len);
        try self.types.append(self.allocator, t);
        return id;
    }

    /// Get a type by ID.
    pub fn get(self: *const TypePool, id: TypeId) ?Type {
        if (id == invalid_type or id >= self.types.items.len) {
            return null;
        }
        return self.types.items[id];
    }

    /// Look up a type by name.
    pub fn getByName(self: *const TypePool, name: []const u8) ?TypeId {
        return self.name_map.get(name);
    }

    /// Register a named type.
    pub fn registerName(self: *TypePool, name: []const u8, id: TypeId) !void {
        const owned_name = try self.allocator.dupe(u8, name);
        try self.name_map.put(self.allocator, owned_name, id);
    }

    /// Create a reference type.
    pub fn makeReference(self: *TypePool, pointee: TypeId, is_mut: bool) !TypeId {
        return self.addType(.{
            .kind = .{ .reference = .{ .pointee = pointee, .is_mut = is_mut } },
            .span = null,
        });
    }

    /// Create an optional type.
    pub fn makeOptional(self: *TypePool, inner: TypeId) !TypeId {
        return self.addType(.{
            .kind = .{ .optional = .{ .inner = inner } },
            .span = null,
        });
    }

    /// Create an array type.
    pub fn makeArray(self: *TypePool, element: TypeId, size: usize) !TypeId {
        return self.addType(.{
            .kind = .{ .array = .{ .element = element, .size = size } },
            .span = null,
        });
    }

    /// Create a slice type.
    pub fn makeSlice(self: *TypePool, element: TypeId) !TypeId {
        return self.addType(.{
            .kind = .{ .slice = .{ .element = element } },
            .span = null,
        });
    }

    /// Create a tuple type.
    pub fn makeTuple(self: *TypePool, elements: []const TypeId) !TypeId {
        const owned = try self.allocator.dupe(TypeId, elements);
        return self.addType(.{
            .kind = .{ .tuple = .{ .elements = owned } },
            .span = null,
        });
    }

    /// Create a function type.
    pub fn makeFunction(self: *TypePool, params: []const TypeId, return_type: TypeId) !TypeId {
        const owned = try self.allocator.dupe(TypeId, params);
        return self.addType(.{
            .kind = .{ .function = .{ .params = owned, .return_type = return_type } },
            .span = null,
        });
    }

    /// Create a generic instantiation type.
    pub fn makeGeneric(self: *TypePool, base: TypeId, args: []const TypeId) !TypeId {
        const owned = try self.allocator.dupe(TypeId, args);
        return self.addType(.{
            .kind = .{ .generic = .{ .base = base, .args = owned } },
            .span = null,
        });
    }

    /// Create an error type for recovery.
    pub fn makeError(self: *TypePool) !TypeId {
        return self.addType(.{
            .kind = .@"error",
            .span = null,
        });
    }

    /// Format a type for display.
    pub fn format(self: *const TypePool, id: TypeId) []const u8 {
        const t = self.get(id) orelse return "<invalid>";
        return self.formatType(t);
    }

    fn formatType(_: *const TypePool, t: Type) []const u8 {
        return switch (t.kind) {
            .primitive => |p| p.toString(),
            .named => |n| n.name,
            .generic => "<generic>",
            .reference => |r| if (r.is_mut) "&mut _" else "&_",
            .optional => "?_",
            .array => "[_; N]",
            .slice => "[_]",
            .tuple => "(_)",
            .function => "fn(_) -> _",
            .@"struct" => |s| s.name,
            .@"enum" => |e| e.name,
            .trait => |tr| tr.name,
            .alias => |a| a.name,
            .type_var => |tv| tv.name,
            .@"error" => "<error>",
        };
    }
};

/// Type compatibility checking.
pub const TypeCompat = enum {
    /// Types are exactly the same.
    equal,
    /// Target can be coerced to source (e.g., i8 -> i32).
    coercible,
    /// Types are incompatible.
    incompatible,
};

/// Check if two types are compatible for assignment.
pub fn checkCompatibility(pool: *const TypePool, source: TypeId, target: TypeId) TypeCompat {
    // Same type is always compatible.
    if (source == target) return .equal;

    // Invalid types are always incompatible.
    if (source == invalid_type or target == invalid_type) return .incompatible;

    const src = pool.get(source) orelse return .incompatible;
    const tgt = pool.get(target) orelse return .incompatible;

    // Error types are compatible with anything (for error recovery).
    if (src.isError() or tgt.isError()) return .coercible;

    // Never type is compatible with anything (it never actually produces a value).
    if (src.isNever()) return .coercible;

    // Check numeric coercion.
    if (src.isInteger() and tgt.isInteger()) {
        return checkIntegerCoercion(src.kind.primitive, tgt.kind.primitive);
    }

    // Check reference coercion (&mut T -> &T).
    if (src.kind == .reference and tgt.kind == .reference) {
        const src_ref = src.kind.reference;
        const tgt_ref = tgt.kind.reference;
        if (src_ref.pointee == tgt_ref.pointee) {
            // &mut T -> &T is allowed, but not &T -> &mut T.
            if (src_ref.is_mut and !tgt_ref.is_mut) return .coercible;
            if (!src_ref.is_mut and tgt_ref.is_mut) return .incompatible;
            return .equal;
        }
    }

    return .incompatible;
}

fn checkIntegerCoercion(src: Primitive, tgt: Primitive) TypeCompat {
    const src_size = src.bitSize() orelse return .incompatible;
    const tgt_size = tgt.bitSize() orelse return .incompatible;

    // Same signedness: smaller can coerce to larger.
    if (src.isSigned() == tgt.isSigned()) {
        if (src_size <= tgt_size) return .coercible;
    }

    // Unsigned can coerce to larger signed.
    if (src.isUnsigned() and tgt.isSigned()) {
        if (src_size < tgt_size) return .coercible;
    }

    return .incompatible;
}

// === Tests ===

test "Primitive type properties" {
    try std.testing.expect(Primitive.i32.isInteger());
    try std.testing.expect(Primitive.i32.isSigned());
    try std.testing.expect(!Primitive.i32.isUnsigned());
    try std.testing.expect(Primitive.i32.isNumeric());
    try std.testing.expect(!Primitive.i32.isFloat());

    try std.testing.expect(Primitive.u64.isInteger());
    try std.testing.expect(Primitive.u64.isUnsigned());
    try std.testing.expect(!Primitive.u64.isSigned());

    try std.testing.expect(Primitive.f64.isFloat());
    try std.testing.expect(Primitive.f64.isNumeric());
    try std.testing.expect(!Primitive.f64.isInteger());

    try std.testing.expect(!Primitive.bool.isNumeric());
    try std.testing.expect(!Primitive.void.isNumeric());
}

test "Primitive bitSize" {
    try std.testing.expectEqual(@as(?u16, 8), Primitive.i8.bitSize());
    try std.testing.expectEqual(@as(?u16, 32), Primitive.i32.bitSize());
    try std.testing.expectEqual(@as(?u16, 64), Primitive.f64.bitSize());
    try std.testing.expectEqual(@as(?u16, null), Primitive.usize.bitSize());
    try std.testing.expectEqual(@as(?u16, 0), Primitive.void.bitSize());
}

test "Primitive fromString" {
    try std.testing.expectEqual(Primitive.i32, Primitive.fromString("i32").?);
    try std.testing.expectEqual(Primitive.bool, Primitive.fromString("bool").?);
    try std.testing.expectEqual(@as(?Primitive, null), Primitive.fromString("unknown"));
}

test "TypePool initialization with builtins" {
    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    // Check builtin types are present.
    const void_type = pool.get(BuiltinTypes.void_type).?;
    try std.testing.expect(void_type.isVoid());

    const i32_type = pool.get(BuiltinTypes.i32_type).?;
    try std.testing.expect(i32_type.isInteger());

    const f64_type = pool.get(BuiltinTypes.f64_type).?;
    try std.testing.expect(f64_type.isFloat());

    // Check name lookup.
    try std.testing.expectEqual(BuiltinTypes.i32_type, pool.getByName("i32").?);
    try std.testing.expectEqual(BuiltinTypes.bool_type, pool.getByName("bool").?);
}

test "TypePool make compound types" {
    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    // Create &i32.
    const ref_i32 = try pool.makeReference(BuiltinTypes.i32_type, false);
    const ref_type = pool.get(ref_i32).?;
    try std.testing.expect(ref_type.kind == .reference);
    try std.testing.expectEqual(BuiltinTypes.i32_type, ref_type.kind.reference.pointee);
    try std.testing.expect(!ref_type.kind.reference.is_mut);

    // Create ?bool.
    const opt_bool = try pool.makeOptional(BuiltinTypes.bool_type);
    const opt_type = pool.get(opt_bool).?;
    try std.testing.expect(opt_type.kind == .optional);

    // Create [i32; 10].
    const arr_type = try pool.makeArray(BuiltinTypes.i32_type, 10);
    const arr = pool.get(arr_type).?;
    try std.testing.expect(arr.kind == .array);
    try std.testing.expectEqual(@as(usize, 10), arr.kind.array.size);

    // Create (i32, bool).
    const tuple_type = try pool.makeTuple(&.{ BuiltinTypes.i32_type, BuiltinTypes.bool_type });
    const tup = pool.get(tuple_type).?;
    try std.testing.expect(tup.kind == .tuple);
    try std.testing.expectEqual(@as(usize, 2), tup.kind.tuple.elements.len);

    // Create fn(i32, i32) -> bool.
    const fn_type = try pool.makeFunction(&.{ BuiltinTypes.i32_type, BuiltinTypes.i32_type }, BuiltinTypes.bool_type);
    const func = pool.get(fn_type).?;
    try std.testing.expect(func.kind == .function);
    try std.testing.expectEqual(@as(usize, 2), func.kind.function.params.len);
}

test "Type compatibility - equal types" {
    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    const result = checkCompatibility(&pool, BuiltinTypes.i32_type, BuiltinTypes.i32_type);
    try std.testing.expectEqual(TypeCompat.equal, result);
}

test "Type compatibility - integer coercion" {
    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    // i8 -> i32 (smaller to larger, same sign) should be coercible.
    const result1 = checkCompatibility(&pool, BuiltinTypes.i8_type, BuiltinTypes.i32_type);
    try std.testing.expectEqual(TypeCompat.coercible, result1);

    // i32 -> i8 (larger to smaller) should be incompatible.
    const result2 = checkCompatibility(&pool, BuiltinTypes.i32_type, BuiltinTypes.i8_type);
    try std.testing.expectEqual(TypeCompat.incompatible, result2);

    // u8 -> i32 (unsigned to larger signed) should be coercible.
    const result3 = checkCompatibility(&pool, BuiltinTypes.u8_type, BuiltinTypes.i32_type);
    try std.testing.expectEqual(TypeCompat.coercible, result3);

    // i32 -> u32 (signed to unsigned, same size) should be incompatible.
    const result4 = checkCompatibility(&pool, BuiltinTypes.i32_type, BuiltinTypes.u32_type);
    try std.testing.expectEqual(TypeCompat.incompatible, result4);
}

test "Type compatibility - reference coercion" {
    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    const ref_i32 = try pool.makeReference(BuiltinTypes.i32_type, false);
    const ref_mut_i32 = try pool.makeReference(BuiltinTypes.i32_type, true);

    // &mut i32 -> &i32 should be coercible.
    const result1 = checkCompatibility(&pool, ref_mut_i32, ref_i32);
    try std.testing.expectEqual(TypeCompat.coercible, result1);

    // &i32 -> &mut i32 should be incompatible.
    const result2 = checkCompatibility(&pool, ref_i32, ref_mut_i32);
    try std.testing.expectEqual(TypeCompat.incompatible, result2);
}

test "Type compatibility - never type" {
    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    // never -> anything should be coercible (never produces a value).
    const result = checkCompatibility(&pool, BuiltinTypes.never_type, BuiltinTypes.i32_type);
    try std.testing.expectEqual(TypeCompat.coercible, result);
}

test "Type compatibility - incompatible types" {
    var pool = try TypePool.init(std.testing.allocator);
    defer pool.deinit();

    // i32 -> bool should be incompatible.
    const result1 = checkCompatibility(&pool, BuiltinTypes.i32_type, BuiltinTypes.bool_type);
    try std.testing.expectEqual(TypeCompat.incompatible, result1);

    // f64 -> i32 should be incompatible.
    const result2 = checkCompatibility(&pool, BuiltinTypes.f64_type, BuiltinTypes.i32_type);
    try std.testing.expectEqual(TypeCompat.incompatible, result2);
}
