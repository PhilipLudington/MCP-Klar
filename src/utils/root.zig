//! Utility types and functions for Klar MCP Server.

pub const position = @import("position.zig");
pub const diagnostic = @import("diagnostic.zig");
pub const json = @import("json.zig");

pub const Position = position.Position;
pub const Span = position.Span;
pub const Diagnostic = diagnostic.Diagnostic;
pub const Severity = diagnostic.Severity;
pub const Hint = diagnostic.Hint;

test {
    _ = position;
    _ = diagnostic;
    _ = json;
}
