//! Klar MCP Server entry point.
//!
//! A Zig-based MCP server providing Klar language intelligence to Claude Code.

const std = @import("std");
const mcp = @import("mcp");
const config = @import("config");
const build_options = @import("build_options");

const log = std.log.scoped(.main);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Load runtime configuration (environment variables and klar.json)
    var runtime_config = try config.RuntimeConfig.init(allocator);
    defer runtime_config.deinit();

    if (build_options.enable_logging or runtime_config.verbose) {
        log.info("Klar MCP Server starting...", .{});
        if (runtime_config.getStdPath()) |std_path| {
            log.info("Standard library path: {s}", .{std_path});
        }
    }

    var server = try mcp.Server.init(allocator, &runtime_config);
    defer server.deinit();

    try server.run();
}
