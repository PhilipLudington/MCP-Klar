//! klar_completions tool - Generate code completions at a position.
//!
//! This tool provides intelligent code completion suggestions at a given
//! cursor position, including variables, functions, types, fields, and keywords.
//! Supports context-aware completions (after `.`, in type positions, etc.).

const std = @import("std");
const Allocator = std.mem.Allocator;
const parser = @import("compiler").parser;
const checker = @import("compiler").checker;
const types = @import("compiler").types;
const analysis = @import("analysis");
const utils = @import("utils");

const Parser = parser.Parser;
const Checker = checker.Checker;
const Symbol = checker.Symbol;
const TypePool = types.TypePool;
const Span = utils.Span;

const CompletionEngine = analysis.CompletionEngine;
const CompletionItem = analysis.CompletionItem;
const CompletionItemKind = analysis.CompletionItemKind;
const CompletionContext = analysis.CompletionContext;
const CompletionResult = analysis.CompletionResult;

const log = std.log.scoped(.klar_completions);

/// Execute the klar_completions tool.
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

    // Get prefix filter (optional, for filtering by typed text)
    const prefix: ?[]const u8 = if (obj.get("prefix")) |prefix_value| blk: {
        break :blk switch (prefix_value) {
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

    // Create completion engine and get completions
    var engine = CompletionEngine.init(allocator, &checker_inst, &ast, source);
    const result = try engine.getCompletions(line, column);

    // Filter by prefix if provided
    var filtered_items: []const CompletionItem = result.items;
    if (prefix) |p_str| {
        if (p_str.len > 0) {
            filtered_items = try analysis.completion.filterByPrefix(allocator, result.items, p_str);
        }
    }

    return buildCompletionsResponse(allocator, file_path, line, column, filtered_items, result, prefix);
}

/// Build the completions response.
fn buildCompletionsResponse(
    allocator: Allocator,
    file_path: []const u8,
    line: u32,
    column: u32,
    items: []const CompletionItem,
    result: CompletionResult,
    prefix: ?[]const u8,
) !std.json.Value {
    // Build human-readable text response
    var text_buf: [4096]u8 = undefined;
    var text_len: usize = 0;

    const context_str = contextToString(result.context);
    const header = std.fmt.bufPrint(text_buf[text_len..], "Completions at {s}:{d}:{d} (context: {s}):\n\n", .{
        file_path,
        line,
        column,
        context_str,
    }) catch "";
    text_len += header.len;

    if (result.receiver_type) |recv| {
        const recv_info = std.fmt.bufPrint(text_buf[text_len..], "Receiver type: {s}\n\n", .{recv}) catch "";
        text_len += recv_info.len;
    }

    const count = @min(items.len, 20); // Show at most 20 in text
    for (items[0..count]) |item| {
        const entry = std.fmt.bufPrint(text_buf[text_len..], "- {s} ({s})", .{
            item.label,
            @tagName(item.kind),
        }) catch "";
        text_len += entry.len;

        if (item.detail) |detail| {
            const detail_str = std.fmt.bufPrint(text_buf[text_len..], ": {s}", .{detail}) catch "";
            text_len += detail_str.len;
        }

        const newline = std.fmt.bufPrint(text_buf[text_len..], "\n", .{}) catch "";
        text_len += newline.len;

        if (text_len >= text_buf.len - 200) break;
    }

    if (items.len > count) {
        const more = std.fmt.bufPrint(text_buf[text_len..], "... and {d} more\n", .{items.len - count}) catch "";
        text_len += more.len;
    }

    if (items.len == 0) {
        const no_completions = std.fmt.bufPrint(text_buf[text_len..], "No completions available.\n", .{}) catch "";
        text_len += no_completions.len;
    } else {
        const total = std.fmt.bufPrint(text_buf[text_len..], "\nTotal: {d} completions\n", .{items.len}) catch "";
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
    try data_obj.put("line", .{ .integer = @intCast(line) });
    try data_obj.put("column", .{ .integer = @intCast(column) });

    const context_copy = try allocator.dupe(u8, context_str);
    try data_obj.put("context", .{ .string = context_copy });

    if (result.receiver_type) |recv| {
        try data_obj.put("receiver_type", .{ .string = recv });
    } else {
        try data_obj.put("receiver_type", .null);
    }

    if (prefix) |p_str| {
        try data_obj.put("prefix", .{ .string = p_str });
    } else {
        try data_obj.put("prefix", .null);
    }

    try data_obj.put("is_incomplete", .{ .bool = result.is_incomplete });
    try data_obj.put("count", .{ .integer = @intCast(items.len) });

    // Build completions array
    var completions_arr = std.json.Array.init(allocator);
    for (items) |item| {
        var item_obj = std.json.ObjectMap.init(allocator);

        const label_copy = try allocator.dupe(u8, item.label);
        try item_obj.put("label", .{ .string = label_copy });

        const kind_name = @tagName(item.kind);
        const kind_copy = try allocator.dupe(u8, kind_name);
        try item_obj.put("kind", .{ .string = kind_copy });

        try item_obj.put("lsp_kind", .{ .integer = @intCast(item.kind.toLspKind()) });

        if (item.detail) |detail| {
            const detail_copy = try allocator.dupe(u8, detail);
            try item_obj.put("detail", .{ .string = detail_copy });
        } else {
            try item_obj.put("detail", .null);
        }

        if (item.documentation) |doc| {
            const doc_copy = try allocator.dupe(u8, doc);
            try item_obj.put("documentation", .{ .string = doc_copy });
        } else {
            try item_obj.put("documentation", .null);
        }

        if (item.insert_text) |insert| {
            const insert_copy = try allocator.dupe(u8, insert);
            try item_obj.put("insert_text", .{ .string = insert_copy });
        } else {
            try item_obj.put("insert_text", .null);
        }

        if (item.sort_text) |sort| {
            const sort_copy = try allocator.dupe(u8, sort);
            try item_obj.put("sort_text", .{ .string = sort_copy });
        } else {
            try item_obj.put("sort_text", .null);
        }

        try item_obj.put("deprecated", .{ .bool = item.deprecated });

        try completions_arr.append(.{ .object = item_obj });
    }
    try data_obj.put("completions", .{ .array = completions_arr });

    try outer.put("data", .{ .object = data_obj });

    return .{ .object = outer };
}

/// Convert CompletionContext to string.
fn contextToString(context: CompletionContext) []const u8 {
    return switch (context) {
        .expression => "expression",
        .dot_access => "dot_access",
        .type_position => "type_position",
        .pattern => "pattern",
        .enum_access => "enum_access",
        .import_path => "import_path",
        .impl_target => "impl_target",
        .parameter => "parameter",
        .statement => "statement",
        .unknown => "unknown",
    };
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

test "execute with valid parameters - expression context" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "let x = " });
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 9 }); // After =

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    try std.testing.expectEqualStrings("expression", data.get("context").?.string);

    const completions = data.get("completions").?.array;
    // Should have some completions (at least keywords)
    try std.testing.expect(completions.items.len > 0);
}

test "execute with type position context" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "let x: " });
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 8 }); // After :

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    try std.testing.expectEqualStrings("type_position", data.get("context").?.string);

    const completions = data.get("completions").?.array;
    // Should have primitive types
    var found_i32 = false;
    for (completions.items) |item| {
        const label = item.object.get("label").?.string;
        if (std.mem.eql(u8, label, "i32")) {
            found_i32 = true;
            break;
        }
    }
    try std.testing.expect(found_i32);
}

test "execute with dot access context" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "point." });
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 7 }); // After .

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    try std.testing.expectEqualStrings("dot_access", data.get("context").?.string);
}

test "execute with prefix filter" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "let x: i" });
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 9 });
    try params.put("prefix", .{ .string = "i" });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    try std.testing.expectEqualStrings("i", data.get("prefix").?.string);

    const completions = data.get("completions").?.array;
    // All completions should start with 'i'
    for (completions.items) |item| {
        const label = item.object.get("label").?.string;
        try std.testing.expect(std.mem.startsWith(u8, label, "i"));
    }
}

test "execute returns lsp_kind" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "let x = " });
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 9 });

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    const completions = data.get("completions").?.array;

    // All completions should have lsp_kind
    for (completions.items) |item| {
        const lsp_kind = item.object.get("lsp_kind");
        try std.testing.expect(lsp_kind != null);
        try std.testing.expect(lsp_kind.?.integer >= 0);
    }
}

test "execute with enum access context" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "Color::" });
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 8 }); // After ::

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    try std.testing.expectEqualStrings("enum_access", data.get("context").?.string);
}

test "execute with return type context" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "fn foo() -> " });
    try params.put("line", .{ .integer = 1 });
    try params.put("column", .{ .integer = 13 }); // After ->

    const result = try execute(allocator, .{ .object = params });
    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);

    const data = obj.get("data").?.object;
    try std.testing.expectEqualStrings("type_position", data.get("context").?.string);
}

test "contextToString all values" {
    try std.testing.expectEqualStrings("expression", contextToString(.expression));
    try std.testing.expectEqualStrings("dot_access", contextToString(.dot_access));
    try std.testing.expectEqualStrings("type_position", contextToString(.type_position));
    try std.testing.expectEqualStrings("pattern", contextToString(.pattern));
    try std.testing.expectEqualStrings("enum_access", contextToString(.enum_access));
    try std.testing.expectEqualStrings("import_path", contextToString(.import_path));
    try std.testing.expectEqualStrings("impl_target", contextToString(.impl_target));
    try std.testing.expectEqualStrings("parameter", contextToString(.parameter));
    try std.testing.expectEqualStrings("statement", contextToString(.statement));
    try std.testing.expectEqualStrings("unknown", contextToString(.unknown));
}
