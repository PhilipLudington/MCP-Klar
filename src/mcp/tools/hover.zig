//! klar_hover tool - Get type and documentation info for a symbol at position.
//!
//! This tool provides hover information for a symbol at a given position,
//! returning type, kind, documentation, and definition location.

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

const log = std.log.scoped(.klar_hover);

/// Result of a hover request.
pub const HoverResult = struct {
    /// Whether a symbol was found at the position.
    found: bool,
    /// The symbol name.
    name: ?[]const u8,
    /// Human-readable type string (e.g., "i32", "fn(i32, i32) -> bool").
    type_string: ?[]const u8,
    /// Symbol kind (e.g., "variable", "function", "struct").
    kind: ?[]const u8,
    /// Documentation comment, if any.
    documentation: ?[]const u8,
    /// Definition location.
    definition: ?DefinitionLocation,

    pub const DefinitionLocation = struct {
        file: []const u8,
        line: u32,
        column: u32,
    };
};

/// Execute the klar_hover tool.
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

    // Get line (required, 1-indexed)
    const line_value = obj.get("line") orelse {
        return makeErrorResponse(allocator, "Missing required parameter: line");
    };
    const line: u32 = switch (line_value) {
        .integer => |i| if (i > 0) @intCast(i) else return makeErrorResponse(allocator, "Line must be positive"),
        else => return makeErrorResponse(allocator, "Parameter 'line' must be an integer"),
    };

    // Get column (required, 1-indexed)
    const column_value = obj.get("column") orelse {
        return makeErrorResponse(allocator, "Missing required parameter: column");
    };
    const column: u32 = switch (column_value) {
        .integer => |i| if (i > 0) @intCast(i) else return makeErrorResponse(allocator, "Column must be positive"),
        else => return makeErrorResponse(allocator, "Parameter 'column' must be an integer"),
    };

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

    // Calculate offset from line and column
    const offset = calculateOffset(source, line, column);

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

    _ = offset; // May be used for more precise lookups in the future

    // Find symbol at position
    const symbol = findSymbolAtPosition(&checker_inst, &ast, source, line, column);

    if (symbol) |sym| {
        return buildHoverResponse(allocator, file_path, sym, &checker_inst.type_pool);
    } else {
        return buildNoSymbolResponse(allocator, file_path, line, column);
    }
}

/// Calculate byte offset from line and column (both 1-indexed).
fn calculateOffset(source: []const u8, target_line: u32, target_column: u32) usize {
    var current_line: u32 = 1;
    var offset: usize = 0;

    while (offset < source.len and current_line < target_line) {
        if (source[offset] == '\n') {
            current_line += 1;
        }
        offset += 1;
    }

    // Now add column offset (1-indexed, so subtract 1)
    const col_offset = if (target_column > 0) target_column - 1 else 0;
    offset += col_offset;

    if (offset > source.len) {
        offset = source.len;
    }

    return offset;
}

/// Find symbol at the given position.
fn findSymbolAtPosition(
    checker_inst: *const Checker,
    ast: *const @import("compiler").ast.Ast,
    source: []const u8,
    line: u32,
    column: u32,
) ?Symbol {
    _ = ast;
    _ = source;

    // Look up identifier at position using the checker's scopes
    // We need to find which scope contains this position and look for symbols

    // For now, we'll use a simpler approach: iterate through all scopes
    // and find symbols that match the position
    for (checker_inst.scopes.items) |scope| {
        var iter = scope.symbols.iterator();
        while (iter.next()) |entry| {
            const sym = entry.value_ptr.*;
            // Check if the position is within the symbol's definition span
            if (positionInSpan(line, column, sym.span)) {
                return sym;
            }
        }
    }

    return null;
}

/// Check if a line/column position is within a span.
fn positionInSpan(line: u32, column: u32, span: Span) bool {
    // Check if position is within the span
    if (line < span.start.line or line > span.end.line) {
        return false;
    }

    if (line == span.start.line and column < span.start.column) {
        return false;
    }

    if (line == span.end.line and column > span.end.column) {
        return false;
    }

    return true;
}

fn buildHoverResponse(
    allocator: Allocator,
    file_path: []const u8,
    symbol: Symbol,
    type_pool: *const TypePool,
) !std.json.Value {
    // Build type string
    const type_string = type_pool.format(symbol.type_id);

    // Build kind string
    const kind_string = switch (symbol.kind) {
        .variable => "variable",
        .parameter => "parameter",
        .function => "function",
        .@"struct" => "struct",
        .@"enum" => "enum",
        .trait => "trait",
        .type_alias => "type alias",
        .generic_param => "type parameter",
        .field => "field",
        .variant => "variant",
    };

    // Build hover text
    var text_buf: [1024]u8 = undefined;
    var text_len: usize = 0;

    // Format: "kind name: type"
    const header = std.fmt.bufPrint(text_buf[text_len..], "{s} `{s}`: {s}\n", .{
        kind_string,
        symbol.name,
        type_string,
    }) catch "";
    text_len += header.len;

    // Add mutability info for variables
    if (symbol.kind == .variable and symbol.is_mutable) {
        const mut_info = std.fmt.bufPrint(text_buf[text_len..], "\n(mutable)\n", .{}) catch "";
        text_len += mut_info.len;
    }

    // Add location info
    const loc_info = std.fmt.bufPrint(text_buf[text_len..], "\nDefined at {s}:{d}:{d}", .{
        file_path,
        symbol.span.start.line,
        symbol.span.start.column,
    }) catch "";
    text_len += loc_info.len;

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
    try data_obj.put("found", .{ .bool = true });

    const name_copy = try allocator.dupe(u8, symbol.name);
    try data_obj.put("name", .{ .string = name_copy });

    const type_copy = try allocator.dupe(u8, type_string);
    try data_obj.put("type", .{ .string = type_copy });

    const kind_copy = try allocator.dupe(u8, kind_string);
    try data_obj.put("kind", .{ .string = kind_copy });

    try data_obj.put("isMutable", .{ .bool = symbol.is_mutable });

    // Add definition location
    var def_obj = std.json.ObjectMap.init(allocator);
    try def_obj.put("file", .{ .string = file_path });
    try def_obj.put("line", .{ .integer = @intCast(symbol.span.start.line) });
    try def_obj.put("column", .{ .integer = @intCast(symbol.span.start.column) });
    try data_obj.put("definition", .{ .object = def_obj });

    try outer.put("data", .{ .object = data_obj });

    return .{ .object = outer };
}

fn buildNoSymbolResponse(
    allocator: Allocator,
    file_path: []const u8,
    line: u32,
    column: u32,
) !std.json.Value {
    var text_buf: [256]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buf, "No symbol found at {s}:{d}:{d}", .{
        file_path, line, column,
    }) catch "No symbol found";

    var content_arr = std.json.Array.init(allocator);
    var content_obj = std.json.ObjectMap.init(allocator);
    try content_obj.put("type", .{ .string = "text" });

    const text_copy = try allocator.dupe(u8, text);
    try content_obj.put("text", .{ .string = text_copy });
    try content_arr.append(.{ .object = content_obj });

    var outer = std.json.ObjectMap.init(allocator);
    try outer.put("content", .{ .array = content_arr });
    try outer.put("isError", .{ .bool = false });

    // Add structured data
    var data_obj = std.json.ObjectMap.init(allocator);
    try data_obj.put("found", .{ .bool = false });
    try data_obj.put("file", .{ .string = file_path });
    try data_obj.put("line", .{ .integer = @intCast(line) });
    try data_obj.put("column", .{ .integer = @intCast(column) });
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

test "execute with missing line parameter" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "let x = 42" });
    // Missing line and column

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(is_error);
}

test "execute with valid position - no symbol" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "let x = 42" });
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 100 }); // Way past end

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const found = data.get("found").?.bool;
    try std.testing.expect(!found);
}

test "execute with valid position - symbol found" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "let x = 42" });
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 5 }); // Position of 'x'

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const found = data.get("found").?.bool;
    try std.testing.expect(found);

    const name = data.get("name").?.string;
    try std.testing.expectEqualStrings("x", name);

    const kind = data.get("kind").?.string;
    try std.testing.expectEqualStrings("variable", kind);
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
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 4 }); // Position of 'add'

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const found = data.get("found").?.bool;
    try std.testing.expect(found);

    const name = data.get("name").?.string;
    try std.testing.expectEqualStrings("add", name);

    const kind = data.get("kind").?.string;
    try std.testing.expectEqualStrings("function", kind);
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
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 8 }); // Position of 'Point'

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const found = data.get("found").?.bool;
    try std.testing.expect(found);

    const name = data.get("name").?.string;
    try std.testing.expectEqualStrings("Point", name);

    const kind = data.get("kind").?.string;
    try std.testing.expectEqualStrings("struct", kind);
}

test "calculateOffset basic" {
    const source = "let x = 42\nlet y = 10\n";

    // Line 1, column 1 -> offset 0
    try std.testing.expectEqual(@as(usize, 0), calculateOffset(source, 1, 1));

    // Line 1, column 5 -> offset 4 (position of 'x')
    try std.testing.expectEqual(@as(usize, 4), calculateOffset(source, 1, 5));

    // Line 2, column 1 -> offset 11 (start of second line)
    try std.testing.expectEqual(@as(usize, 11), calculateOffset(source, 2, 1));

    // Line 2, column 5 -> offset 15 (position of 'y')
    try std.testing.expectEqual(@as(usize, 15), calculateOffset(source, 2, 5));
}

test "positionInSpan basic" {
    const span = Span{
        .start = .{ .line = 1, .column = 5, .offset = 4 },
        .end = .{ .line = 1, .column = 6, .offset = 5 },
        .file_id = 0,
    };

    // Position within span
    try std.testing.expect(positionInSpan(1, 5, span));
    try std.testing.expect(positionInSpan(1, 6, span));

    // Position before span
    try std.testing.expect(!positionInSpan(1, 1, span));
    try std.testing.expect(!positionInSpan(1, 4, span));

    // Position after span
    try std.testing.expect(!positionInSpan(1, 7, span));
    try std.testing.expect(!positionInSpan(1, 10, span));

    // Different line
    try std.testing.expect(!positionInSpan(2, 5, span));
}
