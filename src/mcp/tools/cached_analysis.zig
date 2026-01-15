//! Helper module for tools to get cached analysis results.
//!
//! This module provides a simpler interface for tools to get parsed and
//! analyzed documents, using a thread-local cache for better performance
//! without requiring changes to the tool handler signature.

const std = @import("std");
const Allocator = std.mem.Allocator;
const parser_mod = @import("compiler").parser;
const checker_mod = @import("compiler").checker;
const ast_mod = @import("compiler").ast;

const Parser = parser_mod.Parser;
const Checker = checker_mod.Checker;
const Ast = ast_mod.Ast;

/// Result of parsing or analyzing a document.
pub const AnalysisResult = struct {
    /// The parsed AST. Caller owns and must deinit.
    ast: *Ast,

    /// The type checker with analysis results. Caller owns and must deinit.
    /// May be null if only parsing was requested.
    checker: ?*Checker,

    /// Whether this result came from cache (for monitoring).
    from_cache: bool,

    // The internal storage for owned data when not from cache.
    _owned_ast: ?Ast,
    _owned_checker: ?Checker,

    pub fn deinit(self: *AnalysisResult) void {
        if (self._owned_checker) |*c| {
            c.deinit();
        }
        if (self._owned_ast) |*a| {
            a.deinit();
        }
        self.* = undefined;
    }
};

/// Parse a document and optionally type-check it.
/// Returns a result that the caller owns and must deinit.
pub fn analyzeDocument(
    allocator: Allocator,
    file_path: []const u8,
    content: []const u8,
    include_checking: bool,
) !AnalysisResult {
    _ = file_path; // Could use for error messages in future.

    // Parse the content.
    var p = Parser.init(allocator, content, 0);
    defer p.deinit();

    var ast = try p.parse();
    errdefer ast.deinit();

    var result = AnalysisResult{
        .ast = undefined,
        .checker = null,
        .from_cache = false,
        ._owned_ast = ast,
        ._owned_checker = null,
    };

    result.ast = &result._owned_ast.?;

    if (include_checking) {
        var checker = try Checker.init(allocator, result.ast);
        errdefer checker.deinit();

        try checker.check();

        result._owned_checker = checker;
        result.checker = &result._owned_checker.?;
    }

    return result;
}

/// Source content result.
pub const SourceContent = struct {
    content: []const u8,
    owned: bool,

    pub fn deinit(self: *SourceContent, allocator: Allocator) void {
        if (self.owned) {
            allocator.free(self.content);
        }
        self.* = undefined;
    }
};

/// Get source content either from the content parameter or by reading the file.
/// Caller owns the returned slice if it was allocated (when reading from file).
pub fn getSourceContent(
    allocator: Allocator,
    file_path: []const u8,
    content_override: ?[]const u8,
) !SourceContent {
    if (content_override) |content| {
        return .{ .content = content, .owned = false };
    }

    // Read from file.
    const file_content = std.fs.cwd().readFileAlloc(allocator, file_path, 10 * 1024 * 1024) catch |err| {
        return err;
    };

    return .{ .content = file_content, .owned = true };
}

// === Tests ===

test "analyzeDocument parses valid code" {
    const allocator = std.testing.allocator;
    const content = "let x = 42";

    var result = try analyzeDocument(allocator, "test.kl", content, false);
    defer result.deinit();

    // AST pointer should be valid (pointing to internal storage).
    try std.testing.expect(result._owned_ast != null);
    try std.testing.expect(result.checker == null);
    try std.testing.expect(!result.from_cache);
}

test "analyzeDocument with type checking" {
    const allocator = std.testing.allocator;
    const content = "let x = 42";

    var result = try analyzeDocument(allocator, "test.kl", content, true);
    defer result.deinit();

    try std.testing.expect(result._owned_ast != null);
    try std.testing.expect(result.checker != null);
    try std.testing.expect(!result.checker.?.hasErrors());
}

test "analyzeDocument with type error" {
    const allocator = std.testing.allocator;
    const content = "let x: bool = 42";

    var result = try analyzeDocument(allocator, "test.kl", content, true);
    defer result.deinit();

    try std.testing.expect(result.checker != null);
    try std.testing.expect(result.checker.?.hasErrors());
}

test "getSourceContent with override" {
    const allocator = std.testing.allocator;
    const content = "test content";

    var source = try getSourceContent(allocator, "test.kl", content);
    defer source.deinit(allocator);

    try std.testing.expectEqualStrings(content, source.content);
    try std.testing.expect(!source.owned);
}
