//! klar_check tool - Type check a Klar file and return diagnostics.
//!
//! This tool parses and type-checks Klar source code, returning any errors
//! or warnings as MCP tool output.

const std = @import("std");
const Allocator = std.mem.Allocator;
const checker = @import("compiler").checker;
const utils = @import("utils");
const cached_analysis = @import("cached_analysis.zig");

const Checker = checker.Checker;
const Diagnostic = utils.Diagnostic;
const Span = utils.Span;

const log = std.log.scoped(.klar_check);

/// Result of checking a Klar file.
pub const CheckResult = struct {
    success: bool,
    diagnostics: []const DiagnosticInfo,
    error_count: usize,
    warning_count: usize,

    pub const DiagnosticInfo = struct {
        severity: []const u8,
        message: []const u8,
        file: []const u8,
        line: u32,
        column: u32,
        end_line: u32,
        end_column: u32,
    };
};

/// Execute the klar_check tool.
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

    // Get content override (optional, for unsaved buffers)
    const content_override: ?[]const u8 = if (obj.get("content")) |content_value| blk: {
        break :blk switch (content_value) {
            .string => |s| s,
            else => null,
        };
    } else null;

    // Get the source code using cached_analysis helper
    var source = cached_analysis.getSourceContent(allocator, file_path, content_override) catch |err| {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Failed to read file '{s}': {s}", .{ file_path, @errorName(err) }) catch "Failed to read file";
        return makeErrorResponse(allocator, msg);
    };
    defer source.deinit(allocator);

    // Parse and analyze using cached_analysis helper
    var analysis = cached_analysis.analyzeDocument(allocator, file_path, source.content, true) catch |err| {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Analysis error: {s}", .{@errorName(err)}) catch "Analysis error";
        return makeErrorResponse(allocator, msg);
    };
    defer analysis.deinit();

    // Collect diagnostics
    var diagnostics = std.ArrayListUnmanaged(CheckResult.DiagnosticInfo){};
    defer diagnostics.deinit(allocator);

    var error_count: usize = 0;
    var warning_count: usize = 0;

    // Add parse errors
    for (analysis.ast.errors.items) |parse_error| {
        _ = try makeDiagnosticInfo(allocator, &diagnostics, file_path, parse_error.message, parse_error.span, "error");
        error_count += 1;
    }

    // Add type checker diagnostics
    if (analysis.checker) |checker_inst| {
        for (checker_inst.getDiagnostics()) |diag| {
            const severity_str = diag.severity.toString();
            _ = try makeDiagnosticInfo(allocator, &diagnostics, file_path, diag.message, diag.span, severity_str);

            if (diag.severity == .@"error") {
                error_count += 1;
            } else if (diag.severity == .warning) {
                warning_count += 1;
            }
        }
    }

    // Build response
    return buildSuccessResponse(allocator, file_path, diagnostics.items, error_count, warning_count);
}

fn makeDiagnosticInfo(
    allocator: Allocator,
    diagnostics: *std.ArrayListUnmanaged(CheckResult.DiagnosticInfo),
    file_path: []const u8,
    message: []const u8,
    span: Span,
    severity: []const u8,
) !*CheckResult.DiagnosticInfo {
    const diag_info = CheckResult.DiagnosticInfo{
        .severity = severity,
        .message = message,
        .file = file_path,
        .line = span.start.line,
        .column = span.start.column,
        .end_line = span.end.line,
        .end_column = span.end.column,
    };
    try diagnostics.append(allocator, diag_info);
    return &diagnostics.items[diagnostics.items.len - 1];
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

fn buildSuccessResponse(
    allocator: Allocator,
    file_path: []const u8,
    diagnostics: []const CheckResult.DiagnosticInfo,
    error_count: usize,
    warning_count: usize,
) !std.json.Value {
    // Build diagnostics JSON array
    var diag_arr = std.json.Array.init(allocator);
    for (diagnostics) |diag| {
        var diag_obj = std.json.ObjectMap.init(allocator);
        try diag_obj.put("severity", .{ .string = diag.severity });
        try diag_obj.put("message", .{ .string = diag.message });
        try diag_obj.put("file", .{ .string = diag.file });
        try diag_obj.put("line", .{ .integer = @intCast(diag.line) });
        try diag_obj.put("column", .{ .integer = @intCast(diag.column) });
        try diag_obj.put("endLine", .{ .integer = @intCast(diag.end_line) });
        try diag_obj.put("endColumn", .{ .integer = @intCast(diag.end_column) });
        try diag_arr.append(.{ .object = diag_obj });
    }

    // Build summary text
    var summary_buf: [256]u8 = undefined;
    const summary = std.fmt.bufPrint(&summary_buf, "{d} error{s}, {d} warning{s}", .{
        error_count,
        if (error_count == 1) "" else "s",
        warning_count,
        if (warning_count == 1) "" else "s",
    }) catch "Check complete";

    // Build detailed text output
    var text_buf: [4096]u8 = undefined;
    var text_len: usize = 0;

    if (error_count == 0 and warning_count == 0) {
        const header = std.fmt.bufPrint(text_buf[text_len..], "Check passed for {s}: {s}\n", .{ file_path, summary }) catch "";
        text_len += header.len;
    } else {
        const header = std.fmt.bufPrint(text_buf[text_len..], "Check completed for {s}: {s}\n\n", .{ file_path, summary }) catch "";
        text_len += header.len;

        for (diagnostics) |diag| {
            const line = std.fmt.bufPrint(text_buf[text_len..], "{s}:{d}:{d}: {s}: {s}\n", .{
                diag.file,
                diag.line,
                diag.column,
                diag.severity,
                diag.message,
            }) catch "";
            text_len += line.len;
            if (text_len >= text_buf.len - 100) break; // Leave some space
        }
    }

    // Build MCP response
    var content_arr = std.json.Array.init(allocator);
    var content_obj = std.json.ObjectMap.init(allocator);
    try content_obj.put("type", .{ .string = "text" });

    // Duplicate text to ensure it persists
    const text_copy = try allocator.dupe(u8, text_buf[0..text_len]);
    try content_obj.put("text", .{ .string = text_copy });
    try content_arr.append(.{ .object = content_obj });

    var outer = std.json.ObjectMap.init(allocator);
    try outer.put("content", .{ .array = content_arr });
    try outer.put("isError", .{ .bool = error_count > 0 });

    // Add structured data for programmatic access
    var data_obj = std.json.ObjectMap.init(allocator);
    try data_obj.put("file", .{ .string = file_path });
    try data_obj.put("success", .{ .bool = error_count == 0 });
    try data_obj.put("errorCount", .{ .integer = @intCast(error_count) });
    try data_obj.put("warningCount", .{ .integer = @intCast(warning_count) });
    try data_obj.put("diagnostics", .{ .array = diag_arr });

    // Duplicate summary to ensure it persists
    const summary_copy = try allocator.dupe(u8, summary);
    try data_obj.put("summary", .{ .string = summary_copy });

    try outer.put("data", .{ .object = data_obj });

    return .{ .object = outer };
}

// === Tests ===

/// Helper to run execute with arena allocator for test cleanup.
fn testExecute(params: ?std.json.Value) !struct { result: std.json.Value, arena: std.heap.ArenaAllocator } {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    errdefer arena.deinit();

    const result = try execute(arena.allocator(), params);
    return .{ .result = result, .arena = arena };
}

test "execute with missing parameters" {
    var state = try testExecute(null);
    defer state.arena.deinit();

    const obj = state.result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(is_error);
}

test "execute with valid content parameter" {
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
    const success = data.get("success").?.bool;
    try std.testing.expect(success);
    try std.testing.expectEqual(@as(i64, 0), data.get("errorCount").?.integer);
}

test "execute with type error" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "let x: bool = 42" }); // Type mismatch

    const result = try execute(allocator, .{ .object = params });

    const obj = result.object;
    const data = obj.get("data").?.object;
    const error_count = data.get("errorCount").?.integer;
    try std.testing.expect(error_count > 0);
}

test "execute with undefined identifier" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var params = std.json.ObjectMap.init(allocator);
    try params.put("file", .{ .string = "test.kl" });
    try params.put("content", .{ .string = "let x = undefined_var" });

    const result = try execute(allocator, .{ .object = params });

    const obj = result.object;
    const data = obj.get("data").?.object;
    const error_count = data.get("errorCount").?.integer;
    try std.testing.expect(error_count > 0);
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
    const data = obj.get("data").?.object;
    const success = data.get("success").?.bool;
    try std.testing.expect(success);
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
    const data = obj.get("data").?.object;
    const success = data.get("success").?.bool;
    try std.testing.expect(success);
}
