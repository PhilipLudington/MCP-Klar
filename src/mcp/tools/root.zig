//! MCP tool implementations for Klar language server.

pub const check = @import("check.zig");
pub const hover = @import("hover.zig");

test {
    _ = check;
    _ = hover;
}
