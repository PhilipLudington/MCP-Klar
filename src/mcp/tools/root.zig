//! MCP tool implementations for Klar language server.

pub const check = @import("check.zig");
pub const definition = @import("definition.zig");
pub const hover = @import("hover.zig");
pub const references = @import("references.zig");
pub const symbols = @import("symbols.zig");

test {
    _ = check;
    _ = definition;
    _ = hover;
    _ = references;
    _ = symbols;
}
