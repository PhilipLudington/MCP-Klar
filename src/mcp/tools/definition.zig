//! klar_goto_definition tool - Find the definition location of a symbol at position.
//!
//! This tool provides go-to-definition functionality for a symbol at a given position,
//! returning the file, line, column, kind, and a preview of the definition.

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

const log = std.log.scoped(.klar_definition);

/// Result of a go-to-definition request.
pub const DefinitionResult = struct {
    /// Whether a definition was found.
    found: bool,
    /// Definition locations (may have multiple for traits/interfaces).
    definitions: []const Definition,

    pub const Definition = struct {
        /// File path of the definition.
        file: []const u8,
        /// Line number (1-indexed).
        line: u32,
        /// Column number (1-indexed).
        column: u32,
        /// Symbol kind (e.g., "variable", "function", "struct").
        kind: []const u8,
        /// Symbol name.
        name: []const u8,
        /// Preview of the definition line (trimmed).
        preview: ?[]const u8,
    };
};

/// Execute the klar_goto_definition tool.
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

    // Find symbol at position
    const symbol = findSymbolAtPosition(&checker_inst, source, line, column);

    if (symbol) |sym| {
        return buildDefinitionResponse(allocator, file_path, sym, source);
    } else {
        return buildNoDefinitionResponse(allocator, file_path, line, column);
    }
}

/// Find symbol at the given position.
fn findSymbolAtPosition(
    checker_inst: *const Checker,
    source: []const u8,
    line: u32,
    column: u32,
) ?Symbol {
    _ = source;

    // Look through all scopes for symbols at this position
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

/// Get a preview of the line containing the definition.
fn getLinePreview(allocator: Allocator, source: []const u8, target_line: u32) !?[]const u8 {
    if (target_line == 0) return null;

    var current_line: u32 = 1;
    var line_start: usize = 0;
    var i: usize = 0;

    // Find the start of the target line
    while (i < source.len and current_line < target_line) {
        if (source[i] == '\n') {
            current_line += 1;
            line_start = i + 1;
        }
        i += 1;
    }

    if (current_line != target_line) return null;

    // Find the end of the line
    var line_end = line_start;
    while (line_end < source.len and source[line_end] != '\n') {
        line_end += 1;
    }

    if (line_start >= source.len) return null;

    const line_content = source[line_start..line_end];

    // Trim leading whitespace
    var trimmed_start: usize = 0;
    while (trimmed_start < line_content.len and
        (line_content[trimmed_start] == ' ' or line_content[trimmed_start] == '\t'))
    {
        trimmed_start += 1;
    }

    const trimmed = line_content[trimmed_start..];

    // Limit preview length
    const max_preview_len: usize = 100;
    const preview_len = @min(trimmed.len, max_preview_len);

    return try allocator.dupe(u8, trimmed[0..preview_len]);
}

fn buildDefinitionResponse(
    allocator: Allocator,
    file_path: []const u8,
    symbol: Symbol,
    source: []const u8,
) !std.json.Value {
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

    // Get line preview
    const preview = try getLinePreview(allocator, source, symbol.span.start.line);

    // Build response text
    var text_buf: [512]u8 = undefined;
    const text = if (preview) |p|
        std.fmt.bufPrint(&text_buf, "Definition of `{s}` ({s}) at {s}:{d}:{d}\n\n{s}", .{
            symbol.name,
            kind_string,
            file_path,
            symbol.span.start.line,
            symbol.span.start.column,
            p,
        }) catch "Definition found"
    else
        std.fmt.bufPrint(&text_buf, "Definition of `{s}` ({s}) at {s}:{d}:{d}", .{
            symbol.name,
            kind_string,
            file_path,
            symbol.span.start.line,
            symbol.span.start.column,
        }) catch "Definition found";

    // Build MCP response
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
    try data_obj.put("found", .{ .bool = true });

    // Build definitions array (single definition for now)
    var definitions_arr = std.json.Array.init(allocator);

    var def_obj = std.json.ObjectMap.init(allocator);
    try def_obj.put("file", .{ .string = file_path });
    try def_obj.put("line", .{ .integer = @intCast(symbol.span.start.line) });
    try def_obj.put("column", .{ .integer = @intCast(symbol.span.start.column) });

    const kind_copy = try allocator.dupe(u8, kind_string);
    try def_obj.put("kind", .{ .string = kind_copy });

    const name_copy = try allocator.dupe(u8, symbol.name);
    try def_obj.put("name", .{ .string = name_copy });

    if (preview) |p| {
        try def_obj.put("preview", .{ .string = p });
    } else {
        try def_obj.put("preview", .null);
    }

    try definitions_arr.append(.{ .object = def_obj });
    try data_obj.put("definitions", .{ .array = definitions_arr });

    try outer.put("data", .{ .object = data_obj });

    return .{ .object = outer };
}

fn buildNoDefinitionResponse(
    allocator: Allocator,
    file_path: []const u8,
    line: u32,
    column: u32,
) !std.json.Value {
    var text_buf: [256]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buf, "No definition found at {s}:{d}:{d}", .{
        file_path, line, column,
    }) catch "No definition found";

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

    // Empty definitions array
    const definitions_arr = std.json.Array.init(allocator);
    try data_obj.put("definitions", .{ .array = definitions_arr });

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

    // Should have empty definitions array
    const definitions = data.get("definitions").?.array;
    try std.testing.expectEqual(@as(usize, 0), definitions.items.len);
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

    // Check definitions array
    const definitions = data.get("definitions").?.array;
    try std.testing.expectEqual(@as(usize, 1), definitions.items.len);

    const def = definitions.items[0].object;
    const name = def.get("name").?.string;
    try std.testing.expectEqualStrings("x", name);

    const kind = def.get("kind").?.string;
    try std.testing.expectEqualStrings("variable", kind);

    const def_line = def.get("line").?.integer;
    try std.testing.expectEqual(@as(i64, 1), def_line);
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

    const definitions = data.get("definitions").?.array;
    try std.testing.expectEqual(@as(usize, 1), definitions.items.len);

    const def = definitions.items[0].object;
    const name = def.get("name").?.string;
    try std.testing.expectEqualStrings("add", name);

    const kind = def.get("kind").?.string;
    try std.testing.expectEqualStrings("function", kind);

    // Should have a preview
    const preview = def.get("preview").?;
    try std.testing.expect(preview != .null);
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

    const definitions = data.get("definitions").?.array;
    try std.testing.expectEqual(@as(usize, 1), definitions.items.len);

    const def = definitions.items[0].object;
    const name = def.get("name").?.string;
    try std.testing.expectEqualStrings("Point", name);

    const kind = def.get("kind").?.string;
    try std.testing.expectEqualStrings("struct", kind);
}

test "getLinePreview basic" {
    const source = "fn foo() -> i32 {\n    return 42\n}\n";

    // Line 1
    const preview1 = try getLinePreview(std.testing.allocator, source, 1);
    defer if (preview1) |p| std.testing.allocator.free(p);
    try std.testing.expect(preview1 != null);
    try std.testing.expectEqualStrings("fn foo() -> i32 {", preview1.?);

    // Line 2 (indented)
    const preview2 = try getLinePreview(std.testing.allocator, source, 2);
    defer if (preview2) |p| std.testing.allocator.free(p);
    try std.testing.expect(preview2 != null);
    try std.testing.expectEqualStrings("return 42", preview2.?);

    // Line 3
    const preview3 = try getLinePreview(std.testing.allocator, source, 3);
    defer if (preview3) |p| std.testing.allocator.free(p);
    try std.testing.expect(preview3 != null);
    try std.testing.expectEqualStrings("}", preview3.?);

    // Line 0 (invalid)
    const preview0 = try getLinePreview(std.testing.allocator, source, 0);
    try std.testing.expect(preview0 == null);

    // Line 100 (past end)
    const preview100 = try getLinePreview(std.testing.allocator, source, 100);
    try std.testing.expect(preview100 == null);
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

test "positionInSpan multiline" {
    const span = Span{
        .start = .{ .line = 1, .column = 10, .offset = 9 },
        .end = .{ .line = 3, .column = 5, .offset = 30 },
        .file_id = 0,
    };

    // Start line, before start column
    try std.testing.expect(!positionInSpan(1, 5, span));
    // Start line, at start column
    try std.testing.expect(positionInSpan(1, 10, span));
    // Start line, after start column
    try std.testing.expect(positionInSpan(1, 15, span));

    // Middle line, any column
    try std.testing.expect(positionInSpan(2, 1, span));
    try std.testing.expect(positionInSpan(2, 50, span));

    // End line, before end column
    try std.testing.expect(positionInSpan(3, 1, span));
    // End line, at end column
    try std.testing.expect(positionInSpan(3, 5, span));
    // End line, after end column
    try std.testing.expect(!positionInSpan(3, 10, span));

    // Lines outside
    try std.testing.expect(!positionInSpan(0, 5, span));
    try std.testing.expect(!positionInSpan(4, 5, span));
}
