//! klar_symbols tool - List all symbols in a file.
//!
//! This tool provides document symbol functionality, listing all symbols
//! (functions, structs, enums, variables, etc.) defined in a Klar file.
//! Supports filtering by symbol kind and includes nested children
//! (fields, variants, methods).

const std = @import("std");
const Allocator = std.mem.Allocator;
const parser = @import("compiler").parser;
const checker = @import("compiler").checker;
const types = @import("compiler").types;
const utils = @import("utils");

const Parser = parser.Parser;
const Checker = checker.Checker;
const Symbol = checker.Symbol;
const TypePool = types.TypePool;
const Span = utils.Span;

const log = std.log.scoped(.klar_symbols);

/// A symbol entry with optional children (for nested symbols like struct fields).
pub const SymbolInfo = struct {
    /// Symbol name.
    name: []const u8,
    /// Symbol kind (function, struct, enum, variable, etc.).
    kind: []const u8,
    /// Type string representation.
    type_string: []const u8,
    /// Line number (1-indexed).
    line: u32,
    /// Column number (1-indexed).
    column: u32,
    /// End line (for range).
    end_line: u32,
    /// End column (for range).
    end_column: u32,
    /// Whether the symbol is public.
    is_public: bool,
    /// Whether the symbol is mutable (for variables).
    is_mutable: bool,
    /// Nested children (fields, variants, methods).
    children: []const SymbolInfo,
};

/// Result of a symbols request.
pub const SymbolsResult = struct {
    /// List of top-level symbols.
    symbols: []const SymbolInfo,
    /// Total count including nested symbols.
    total_count: usize,
    /// Filter applied, if any.
    filter: ?[]const u8,
};

/// Execute the klar_symbols tool.
pub fn execute(allocator: Allocator, params: ?std.json.Value) !std.json.Value {
    // Extract parameters
    const p = params orelse {
        return makeErrorResponse(allocator, "Missing parameters");
    };

    const obj = switch (p) {
        .object => |o| o,
        else => return makeErrorResponse(allocator, "Parameters must be an object"),
    };

    // Get file path (required)
    const file_value = obj.get("file") orelse {
        return makeErrorResponse(allocator, "Missing required parameter: file");
    };
    const file_path = switch (file_value) {
        .string => |s| s,
        else => return makeErrorResponse(allocator, "Parameter 'file' must be a string"),
    };

    // Get kind filter (optional)
    const kind_filter: ?[]const u8 = if (obj.get("kind")) |kind_value| blk: {
        break :blk switch (kind_value) {
            .string => |s| s,
            else => null,
        };
    } else null;

    // Get content override (optional, for unsaved buffers)
    const content: ?[]const u8 = if (obj.get("content")) |content_value| blk: {
        break :blk switch (content_value) {
            .string => |s| s,
            else => null,
        };
    } else null;

    // Get the source code
    const source = if (content) |c| c else blk: {
        // Read from file
        const file_content = std.fs.cwd().readFileAlloc(allocator, file_path, 10 * 1024 * 1024) catch |err| {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Failed to read file '{s}': {s}", .{ file_path, @errorName(err) }) catch "Failed to read file";
            return makeErrorResponse(allocator, msg);
        };
        break :blk file_content;
    };
    defer if (content == null) allocator.free(source);

    // Parse the source
    var p_instance = Parser.init(allocator, source, 0);
    defer p_instance.deinit();

    var ast = p_instance.parse() catch |err| {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Parse error: {s}", .{@errorName(err)}) catch "Parse error";
        return makeErrorResponse(allocator, msg);
    };
    defer ast.deinit();

    // Run type checker
    var checker_inst = Checker.init(allocator, &ast) catch |err| {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Checker initialization error: {s}", .{@errorName(err)}) catch "Checker error";
        return makeErrorResponse(allocator, msg);
    };
    defer checker_inst.deinit();

    checker_inst.check() catch {};

    // Collect symbols
    return buildSymbolsResponse(allocator, file_path, &checker_inst, kind_filter);
}

/// Convert Symbol.Kind to string.
fn kindToString(kind: Symbol.Kind) []const u8 {
    return switch (kind) {
        .variable => "variable",
        .parameter => "parameter",
        .function => "function",
        .@"struct" => "struct",
        .@"enum" => "enum",
        .trait => "trait",
        .type_alias => "type_alias",
        .generic_param => "type_parameter",
        .field => "field",
        .variant => "variant",
    };
}

/// Check if a kind string matches the filter.
fn matchesFilter(kind_str: []const u8, filter: ?[]const u8) bool {
    if (filter) |f| {
        return std.mem.eql(u8, kind_str, f);
    }
    return true;
}

/// Build the symbols response.
fn buildSymbolsResponse(
    allocator: Allocator,
    file_path: []const u8,
    checker_inst: *const Checker,
    kind_filter: ?[]const u8,
) !std.json.Value {
    // Collect all symbols from the checker's scopes
    var symbols_list = std.ArrayListUnmanaged(SymbolInfo){};
    defer symbols_list.deinit(allocator);

    var total_count: usize = 0;

    // Get the global scope (first scope)
    if (checker_inst.scopes.items.len > 0) {
        const global_scope = &checker_inst.scopes.items[0];
        var iter = global_scope.symbols.iterator();

        while (iter.next()) |entry| {
            const sym = entry.value_ptr.*;
            const kind_str = kindToString(sym.kind);

            if (!matchesFilter(kind_str, kind_filter)) {
                continue;
            }

            const type_string = checker_inst.type_pool.format(sym.type_id);

            try symbols_list.append(allocator, .{
                .name = sym.name,
                .kind = kind_str,
                .type_string = type_string,
                .line = sym.span.start.line,
                .column = sym.span.start.column,
                .end_line = sym.span.end.line,
                .end_column = sym.span.end.column,
                .is_public = false, // TODO: track visibility
                .is_mutable = sym.is_mutable,
                .children = &.{}, // TODO: populate children for structs/enums
            });

            total_count += 1;
        }
    }

    // Also collect symbols from nested scopes (for completeness)
    for (checker_inst.scopes.items[1..]) |scope| {
        var iter = scope.symbols.iterator();

        while (iter.next()) |entry| {
            const sym = entry.value_ptr.*;
            const kind_str = kindToString(sym.kind);

            // For nested scopes, only include if filter matches or no filter
            if (!matchesFilter(kind_str, kind_filter)) {
                continue;
            }

            // Skip parameters and local variables unless specifically filtered
            if (kind_filter == null and (sym.kind == .parameter or sym.kind == .variable)) {
                continue;
            }

            const type_string = checker_inst.type_pool.format(sym.type_id);

            try symbols_list.append(allocator, .{
                .name = sym.name,
                .kind = kind_str,
                .type_string = type_string,
                .line = sym.span.start.line,
                .column = sym.span.start.column,
                .end_line = sym.span.end.line,
                .end_column = sym.span.end.column,
                .is_public = false,
                .is_mutable = sym.is_mutable,
                .children = &.{},
            });

            total_count += 1;
        }
    }

    // Sort symbols by line number
    std.mem.sort(SymbolInfo, symbols_list.items, {}, struct {
        fn lessThan(_: void, a: SymbolInfo, b: SymbolInfo) bool {
            if (a.line != b.line) return a.line < b.line;
            return a.column < b.column;
        }
    }.lessThan);

    // Build human-readable text response
    var text_buf: [4096]u8 = undefined;
    var text_len: usize = 0;

    if (kind_filter) |f| {
        const header = std.fmt.bufPrint(text_buf[text_len..], "Symbols in {s} (filtered by kind: {s}):\n\n", .{
            file_path,
            f,
        }) catch "";
        text_len += header.len;
    } else {
        const header = std.fmt.bufPrint(text_buf[text_len..], "Symbols in {s}:\n\n", .{file_path}) catch "";
        text_len += header.len;
    }

    for (symbols_list.items) |sym| {
        const entry = std.fmt.bufPrint(text_buf[text_len..], "- {s} `{s}`: {s} (line {d})\n", .{
            sym.kind,
            sym.name,
            sym.type_string,
            sym.line,
        }) catch "";
        text_len += entry.len;
        if (text_len >= text_buf.len - 200) {
            const truncated = std.fmt.bufPrint(text_buf[text_len..], "... ({d} more symbols)\n", .{
                symbols_list.items.len - total_count,
            }) catch "";
            text_len += truncated.len;
            break;
        }
    }

    if (symbols_list.items.len == 0) {
        const no_symbols = std.fmt.bufPrint(text_buf[text_len..], "No symbols found.\n", .{}) catch "";
        text_len += no_symbols.len;
    } else {
        const total = std.fmt.bufPrint(text_buf[text_len..], "\nTotal: {d} symbols\n", .{total_count}) catch "";
        text_len += total.len;
    }

    // Build MCP response
    var content_arr = std.json.Array.init(allocator);
    var content_obj = std.json.ObjectMap.init(allocator);
    try content_obj.put("type", .{ .string = "text" });

    const text_copy = try allocator.dupe(u8, text_buf[0..text_len]);
    try content_obj.put("text", .{ .string = text_copy });
    try content_arr.append(.{ .object = content_obj });

    var outer = std.json.ObjectMap.init(allocator);
    try outer.put("content", .{ .array = content_arr });
    try outer.put("isError", .{ .bool = false });

    // Add structured data
    var data_obj = std.json.ObjectMap.init(allocator);
    try data_obj.put("file", .{ .string = file_path });
    try data_obj.put("total_count", .{ .integer = @intCast(total_count) });

    if (kind_filter) |f| {
        try data_obj.put("filter", .{ .string = f });
    } else {
        try data_obj.put("filter", .null);
    }

    // Build symbols array
    var symbols_arr = std.json.Array.init(allocator);
    for (symbols_list.items) |sym| {
        var sym_obj = std.json.ObjectMap.init(allocator);

        const name_copy = try allocator.dupe(u8, sym.name);
        try sym_obj.put("name", .{ .string = name_copy });

        const kind_copy = try allocator.dupe(u8, sym.kind);
        try sym_obj.put("kind", .{ .string = kind_copy });

        const type_copy = try allocator.dupe(u8, sym.type_string);
        try sym_obj.put("type", .{ .string = type_copy });

        try sym_obj.put("line", .{ .integer = @intCast(sym.line) });
        try sym_obj.put("column", .{ .integer = @intCast(sym.column) });
        try sym_obj.put("end_line", .{ .integer = @intCast(sym.end_line) });
        try sym_obj.put("end_column", .{ .integer = @intCast(sym.end_column) });
        try sym_obj.put("is_public", .{ .bool = sym.is_public });
        try sym_obj.put("is_mutable", .{ .bool = sym.is_mutable });

        // Empty children array for now
        const children_arr = std.json.Array.init(allocator);
        try sym_obj.put("children", .{ .array = children_arr });

        try symbols_arr.append(.{ .object = sym_obj });
    }
    try data_obj.put("symbols", .{ .array = symbols_arr });

    try outer.put("data", .{ .object = data_obj });

    return .{ .object = outer };
}

fn makeErrorResponse(allocator: Allocator, message: []const u8) !std.json.Value {
    var content_arr = std.json.Array.init(allocator);
    var content_obj = std.json.ObjectMap.init(allocator);
    try content_obj.put("type", .{ .string = "text" });
    try content_obj.put("text", .{ .string = message });
    try content_arr.append(.{ .object = content_obj });

    var outer = std.json.ObjectMap.init(allocator);
    try outer.put("content", .{ .array = content_arr });
    try outer.put("isError", .{ .bool = true });

    return .{ .object = outer };
}

// === Tests ===

test "execute with missing parameters" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const result = try execute(arena.allocator(), null);
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(is_error);
}

test "execute with missing file parameter" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const params = std.json.ObjectMap.init(allocator);
    // Missing file parameter

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(is_error);
}

test "execute with empty file" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "" });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const total_count = data.get("total_count").?.integer;
    try std.testing.expectEqual(@as(i64, 0), total_count);
}

test "execute with single variable" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "let x = 42" });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const total_count = data.get("total_count").?.integer;
    try std.testing.expectEqual(@as(i64, 1), total_count);

    const symbols = data.get("symbols").?.array;
    try std.testing.expectEqual(@as(usize, 1), symbols.items.len);

    const sym = symbols.items[0].object;
    try std.testing.expectEqualStrings("x", sym.get("name").?.string);
    try std.testing.expectEqualStrings("variable", sym.get("kind").?.string);
}

test "execute with function declaration" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string =
        \\fn add(a: i32, b: i32) -> i32 {
        \\    a + b
        \\}
    });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const symbols = data.get("symbols").?.array;

    // Should have the function
    var found_function = false;
    for (symbols.items) |item| {
        const sym = item.object;
        if (std.mem.eql(u8, sym.get("kind").?.string, "function")) {
            try std.testing.expectEqualStrings("add", sym.get("name").?.string);
            found_function = true;
        }
    }
    try std.testing.expect(found_function);
}

test "execute with struct declaration" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string =
        \\struct Point {
        \\    x: i32,
        \\    y: i32
        \\}
    });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const symbols = data.get("symbols").?.array;

    // Should have the struct
    var found_struct = false;
    for (symbols.items) |item| {
        const sym = item.object;
        if (std.mem.eql(u8, sym.get("kind").?.string, "struct")) {
            try std.testing.expectEqualStrings("Point", sym.get("name").?.string);
            found_struct = true;
        }
    }
    try std.testing.expect(found_struct);
}

test "execute with kind filter - function" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string =
        \\let x = 42
        \\fn foo() -> i32 { x }
        \\struct Bar {}
    });
    try params.put("kind", .{ .string = "function" });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const filter = data.get("filter").?.string;
    try std.testing.expectEqualStrings("function", filter);

    const symbols = data.get("symbols").?.array;
    // Should only have functions
    for (symbols.items) |item| {
        const sym = item.object;
        try std.testing.expectEqualStrings("function", sym.get("kind").?.string);
    }
}

test "execute with kind filter - struct" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string =
        \\let x = 42
        \\fn foo() -> i32 { x }
        \\struct Bar {}
    });
    try params.put("kind", .{ .string = "struct" });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const symbols = data.get("symbols").?.array;

    // Should only have structs
    for (symbols.items) |item| {
        const sym = item.object;
        try std.testing.expectEqualStrings("struct", sym.get("kind").?.string);
    }
}

test "execute with kind filter - variable" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string =
        \\let x = 42
        \\let y = 10
        \\fn foo() -> i32 { x }
    });
    try params.put("kind", .{ .string = "variable" });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const symbols = data.get("symbols").?.array;

    // Should only have variables
    for (symbols.items) |item| {
        const sym = item.object;
        try std.testing.expectEqualStrings("variable", sym.get("kind").?.string);
    }
}

test "execute with enum declaration" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string =
        \\enum Color {
        \\    Red,
        \\    Green,
        \\    Blue
        \\}
    });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const symbols = data.get("symbols").?.array;

    // Should have the enum
    var found_enum = false;
    for (symbols.items) |item| {
        const sym = item.object;
        if (std.mem.eql(u8, sym.get("kind").?.string, "enum")) {
            try std.testing.expectEqualStrings("Color", sym.get("name").?.string);
            found_enum = true;
        }
    }
    try std.testing.expect(found_enum);
}

test "execute returns correct line and column" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string =
        \\let first = 1
        \\let second = 2
    });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const symbols = data.get("symbols").?.array;

    // Find 'first' and 'second' symbols
    var first_line: ?i64 = null;
    var second_line: ?i64 = null;

    for (symbols.items) |item| {
        const sym = item.object;
        const name = sym.get("name").?.string;
        if (std.mem.eql(u8, name, "first")) {
            first_line = sym.get("line").?.integer;
        } else if (std.mem.eql(u8, name, "second")) {
            second_line = sym.get("line").?.integer;
        }
    }

    try std.testing.expect(first_line != null);
    try std.testing.expect(second_line != null);
    try std.testing.expectEqual(@as(i64, 1), first_line.?);
    try std.testing.expectEqual(@as(i64, 2), second_line.?);
}

test "kindToString covers all kinds" {
    try std.testing.expectEqualStrings("variable", kindToString(.variable));
    try std.testing.expectEqualStrings("parameter", kindToString(.parameter));
    try std.testing.expectEqualStrings("function", kindToString(.function));
    try std.testing.expectEqualStrings("struct", kindToString(.@"struct"));
    try std.testing.expectEqualStrings("enum", kindToString(.@"enum"));
    try std.testing.expectEqualStrings("trait", kindToString(.trait));
    try std.testing.expectEqualStrings("type_alias", kindToString(.type_alias));
    try std.testing.expectEqualStrings("type_parameter", kindToString(.generic_param));
    try std.testing.expectEqualStrings("field", kindToString(.field));
    try std.testing.expectEqualStrings("variant", kindToString(.variant));
}

test "matchesFilter with null filter" {
    try std.testing.expect(matchesFilter("function", null));
    try std.testing.expect(matchesFilter("variable", null));
    try std.testing.expect(matchesFilter("struct", null));
}

test "matchesFilter with specific filter" {
    try std.testing.expect(matchesFilter("function", "function"));
    try std.testing.expect(!matchesFilter("variable", "function"));
    try std.testing.expect(!matchesFilter("struct", "function"));
}
