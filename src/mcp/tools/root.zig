//! MCP tool implementations for Klar language server.

pub const check = @import("check.zig");
pub const completions = @import("completions.zig");
pub const definition = @import("definition.zig");
pub const hover = @import("hover.zig");
pub const references = @import("references.zig");
pub const symbols = @import("symbols.zig");
pub const cached_analysis = @import("cached_analysis.zig");

test {
    _ = check;
    _ = completions;
    _ = definition;
    _ = hover;
    _ = references;
    _ = symbols;
    _ = cached_analysis;
}
