//! klar_references tool - Find all usages of a symbol at position.
//!
//! This tool provides find-all-references functionality for a symbol at a given
//! position, returning all locations where the symbol is used in the codebase.

const std = @import("std");
const Allocator = std.mem.Allocator;
const parser = @import("compiler").parser;
const checker = @import("compiler").checker;
const types = @import("compiler").types;
const utils = @import("utils");
const symbols_mod = @import("analysis").symbols;
const references_mod = @import("analysis").references;

const Parser = parser.Parser;
const Checker = checker.Checker;
const Symbol = checker.Symbol;
const TypePool = types.TypePool;
const Span = utils.Span;
const SymbolTable = symbols_mod.SymbolTable;
const SymbolReference = symbols_mod.SymbolReference;
const ReferenceIndex = references_mod.ReferenceIndex;

const log = std.log.scoped(.klar_references);

/// Result of a find-references request.
pub const ReferencesResult = struct {
    /// Whether a symbol was found at the position.
    found: bool,
    /// The symbol name (if found).
    symbol_name: ?[]const u8,
    /// All references to the symbol.
    references: []const Reference,
    /// Total count of references.
    total_count: usize,
    /// Whether the definition was included.
    include_definition: bool,
    /// Definition location (if include_definition is true).
    definition: ?Definition,

    pub const Reference = struct {
        /// File path containing this reference.
        file: []const u8,
        /// Line number (1-indexed).
        line: u32,
        /// Column number (1-indexed).
        column: u32,
        /// Kind of reference (read, write, call, type_ref, import).
        kind: []const u8,
        /// Context line (trimmed).
        context: ?[]const u8,
    };

    pub const Definition = struct {
        /// File path containing the definition.
        file: []const u8,
        /// Line number (1-indexed).
        line: u32,
        /// Column number (1-indexed).
        column: u32,
    };
};

/// Execute the klar_references tool.
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

    // Get include_definition (optional, defaults to true)
    const include_definition: bool = if (obj.get("include_definition")) |inc_value| blk: {
        break :blk switch (inc_value) {
            .bool => |b| b,
            else => true,
        };
    } else true;

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
        return buildReferencesResponse(allocator, file_path, sym, &checker_inst, source, include_definition);
    } else {
        return buildNoSymbolResponse(allocator, file_path, line, column);
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

/// Get a preview of the line containing a position.
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

/// Convert reference kind to string.
fn kindToString(kind: SymbolReference.Kind) []const u8 {
    return switch (kind) {
        .read => "read",
        .write => "write",
        .call => "call",
        .type_ref => "type_ref",
        .import => "import",
    };
}

fn buildReferencesResponse(
    allocator: Allocator,
    file_path: []const u8,
    symbol: Symbol,
    checker_inst: *const Checker,
    source: []const u8,
    include_definition: bool,
) !std.json.Value {
    // Collect references to this symbol by matching symbol name
    // Note: In a full implementation, we'd use the symbol table's reference tracking.
    // For now, we'll look through checker's recorded information.
    var refs_list = std.ArrayListUnmanaged(ReferencesResult.Reference){};
    defer refs_list.deinit(allocator);

    // Look through scopes for any references to this symbol
    // This is a simplified approach - in a full implementation,
    // the checker would track references during analysis.
    for (checker_inst.scopes.items) |scope| {
        var iter = scope.symbols.iterator();
        while (iter.next()) |entry| {
            const sym = entry.value_ptr.*;
            // For demonstration, we count uses of variables with same name
            // (In full impl, we'd track actual usages via symbol table)
            if (std.mem.eql(u8, sym.name, symbol.name) and
                sym.span.start.line != symbol.span.start.line)
            {
                const preview = try getLinePreview(allocator, source, sym.span.start.line);
                try refs_list.append(allocator, .{
                    .file = file_path,
                    .line = sym.span.start.line,
                    .column = sym.span.start.column,
                    .kind = "read", // Default to read for now
                    .context = preview,
                });
            }
        }
    }

    // Build definition info
    var definition_info: ?ReferencesResult.Definition = null;
    if (include_definition) {
        definition_info = .{
            .file = file_path,
            .line = symbol.span.start.line,
            .column = symbol.span.start.column,
        };
    }

    const total_count = refs_list.items.len + (if (include_definition) @as(usize, 1) else 0);

    // Build human-readable text response
    var text_buf: [1024]u8 = undefined;
    var text_len: usize = 0;

    if (include_definition) {
        const header = std.fmt.bufPrint(text_buf[text_len..], "References to `{s}` ({d} total, including definition):\n\n", .{
            symbol.name,
            total_count,
        }) catch "";
        text_len += header.len;

        const def_text = std.fmt.bufPrint(text_buf[text_len..], "Definition: {s}:{d}:{d}\n", .{
            file_path,
            symbol.span.start.line,
            symbol.span.start.column,
        }) catch "";
        text_len += def_text.len;
    } else {
        const header = std.fmt.bufPrint(text_buf[text_len..], "References to `{s}` ({d} usages):\n\n", .{
            symbol.name,
            total_count,
        }) catch "";
        text_len += header.len;
    }

    // Add references to text
    for (refs_list.items) |ref| {
        const ref_text = std.fmt.bufPrint(text_buf[text_len..], "- {s}:{d}:{d} ({s})\n", .{
            ref.file,
            ref.line,
            ref.column,
            ref.kind,
        }) catch "";
        text_len += ref_text.len;
        if (text_len >= text_buf.len - 100) break;
    }

    if (refs_list.items.len == 0 and !include_definition) {
        const no_refs = std.fmt.bufPrint(text_buf[text_len..], "No references found.\n", .{}) catch "";
        text_len += no_refs.len;
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
    try data_obj.put("found", .{ .bool = true });

    const name_copy = try allocator.dupe(u8, symbol.name);
    try data_obj.put("symbol_name", .{ .string = name_copy });
    try data_obj.put("total_count", .{ .integer = @intCast(total_count) });
    try data_obj.put("include_definition", .{ .bool = include_definition });

    // Build references array
    var refs_arr = std.json.Array.init(allocator);
    for (refs_list.items) |ref| {
        var ref_obj = std.json.ObjectMap.init(allocator);
        try ref_obj.put("file", .{ .string = ref.file });
        try ref_obj.put("line", .{ .integer = @intCast(ref.line) });
        try ref_obj.put("column", .{ .integer = @intCast(ref.column) });
        try ref_obj.put("kind", .{ .string = ref.kind });
        if (ref.context) |ctx| {
            try ref_obj.put("context", .{ .string = ctx });
        } else {
            try ref_obj.put("context", .null);
        }
        try refs_arr.append(.{ .object = ref_obj });
    }
    try data_obj.put("references", .{ .array = refs_arr });

    // Add definition if requested
    if (definition_info) |def| {
        var def_obj = std.json.ObjectMap.init(allocator);
        try def_obj.put("file", .{ .string = def.file });
        try def_obj.put("line", .{ .integer = @intCast(def.line) });
        try def_obj.put("column", .{ .integer = @intCast(def.column) });
        try data_obj.put("definition", .{ .object = def_obj });
    } else {
        try data_obj.put("definition", .null);
    }

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

    // Empty references array
    const refs_arr = std.json.Array.init(allocator);
    try data_obj.put("references", .{ .array = refs_arr });

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

    // Should have empty references array
    const refs = data.get("references").?.array;
    try std.testing.expectEqual(@as(usize, 0), refs.items.len);
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

    const name = data.get("symbol_name").?.string;
    try std.testing.expectEqualStrings("x", name);

    // Should have definition included by default
    const include_def = data.get("include_definition").?.bool;
    try std.testing.expect(include_def);

    const definition = data.get("definition");
    try std.testing.expect(definition != null);
    try std.testing.expect(definition.? != .null);
}

test "execute with include_definition false" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "let x = 42" });
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 5 }); // Position of 'x'
    try params.put("include_definition", .{ .bool = false });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const include_def = data.get("include_definition").?.bool;
    try std.testing.expect(!include_def);

    const definition = data.get("definition").?;
    try std.testing.expect(definition == .null);
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

    const name = data.get("symbol_name").?.string;
    try std.testing.expectEqualStrings("add", name);
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

test "kindToString" {
    try std.testing.expectEqualStrings("read", kindToString(.read));
    try std.testing.expectEqualStrings("write", kindToString(.write));
    try std.testing.expectEqualStrings("call", kindToString(.call));
    try std.testing.expectEqualStrings("type_ref", kindToString(.type_ref));
    try std.testing.expectEqualStrings("import", kindToString(.import));
}
