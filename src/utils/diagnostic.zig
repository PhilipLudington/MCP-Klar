//! Diagnostic types for reporting errors, warnings, and hints.

const std = @import("std");
const position = @import("position.zig");

const Span = position.Span;
const Allocator = std.mem.Allocator;

/// Severity level of a diagnostic.
pub const Severity = enum {
    @"error",
    warning,
    info,
    hint,

    pub fn toString(self: Severity) []const u8 {
        return switch (self) {
            .@"error" => "error",
            .warning => "warning",
            .info => "info",
            .hint => "hint",
        };
    }
};

/// A hint associated with a diagnostic.
pub const Hint = struct {
    message: []const u8,
    span: ?Span,
};

/// A diagnostic message with location information.
pub const Diagnostic = struct {
    severity: Severity,
    message: []const u8,
    span: Span,
    code: ?[]const u8,
    hints: []const Hint,

    /// Creates an error diagnostic.
    pub fn err(message: []const u8, span: Span) Diagnostic {
        return .{
            .severity = .@"error",
            .message = message,
            .span = span,
            .code = null,
            .hints = &.{},
        };
    }

    /// Creates a warning diagnostic.
    pub fn warn(message: []const u8, span: Span) Diagnostic {
        return .{
            .severity = .warning,
            .message = message,
            .span = span,
            .code = null,
            .hints = &.{},
        };
    }

    /// Creates an info diagnostic.
    pub fn info(message: []const u8, span: Span) Diagnostic {
        return .{
            .severity = .info,
            .message = message,
            .span = span,
            .code = null,
            .hints = &.{},
        };
    }

    /// Creates a diagnostic with an error code.
    pub fn withCode(self: Diagnostic, code: []const u8) Diagnostic {
        var result = self;
        result.code = code;
        return result;
    }

    /// Creates a diagnostic with hints.
    pub fn withHints(self: Diagnostic, hints: []const Hint) Diagnostic {
        var result = self;
        result.hints = hints;
        return result;
    }
};

/// A collection of diagnostics.
pub const DiagnosticList = struct {
    allocator: Allocator,
    items: std.ArrayListUnmanaged(Diagnostic),
    has_errors: bool,

    pub fn init(allocator: Allocator) DiagnosticList {
        return .{
            .allocator = allocator,
            .items = .{},
            .has_errors = false,
        };
    }

    pub fn deinit(self: *DiagnosticList) void {
        self.items.deinit(self.allocator);
    }

    pub fn add(self: *DiagnosticList, diag: Diagnostic) !void {
        try self.items.append(self.allocator, diag);
        if (diag.severity == .@"error") {
            self.has_errors = true;
        }
    }

    pub fn addError(self: *DiagnosticList, message: []const u8, span: Span) !void {
        try self.add(Diagnostic.err(message, span));
    }

    pub fn addWarning(self: *DiagnosticList, message: []const u8, span: Span) !void {
        try self.add(Diagnostic.warn(message, span));
    }

    pub fn count(self: *const DiagnosticList) usize {
        return self.items.items.len;
    }

    pub fn errorCount(self: *const DiagnosticList) usize {
        var n: usize = 0;
        for (self.items.items) |d| {
            if (d.severity == .@"error") n += 1;
        }
        return n;
    }

    pub fn warningCount(self: *const DiagnosticList) usize {
        var n: usize = 0;
        for (self.items.items) |d| {
            if (d.severity == .warning) n += 1;
        }
        return n;
    }
};

test "Diagnostic.err" {
    const span = Span.empty;
    const diag = Diagnostic.err("test error", span);
    try std.testing.expectEqual(Severity.@"error", diag.severity);
    try std.testing.expectEqualStrings("test error", diag.message);
}

test "DiagnosticList basic operations" {
    var list = DiagnosticList.init(std.testing.allocator);
    defer list.deinit();

    try list.addError("error 1", Span.empty);
    try list.addWarning("warning 1", Span.empty);

    try std.testing.expectEqual(@as(usize, 2), list.count());
    try std.testing.expectEqual(@as(usize, 1), list.errorCount());
    try std.testing.expectEqual(@as(usize, 1), list.warningCount());
    try std.testing.expect(list.has_errors);
}
