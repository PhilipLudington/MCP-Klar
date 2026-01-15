//! Klar MCP Server entry point.
//!
//! A Zig-based MCP server providing Klar language intelligence to Claude Code.

const std = @import("std");
const mcp = @import("mcp");
const config = @import("config");

const log = std.log.scoped(.main);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    if (config.enable_logging) {
        log.info("Klar MCP Server starting...", .{});
    }

    var server = try mcp.Server.init(allocator);
    defer server.deinit();

    try server.run();
}
