//! MCP protocol implementation for Klar language server.

pub const protocol = @import("protocol.zig");
pub const transport = @import("transport.zig");
pub const router = @import("router.zig");
pub const server = @import("server.zig");
pub const tools = @import("tools/root.zig");
pub const resources = @import("resources/root.zig");

pub const Server = server.Server;
pub const Request = protocol.Request;
pub const Response = protocol.Response;

test {
    _ = protocol;
    _ = transport;
    _ = router;
    _ = server;
    _ = tools;
    _ = resources;
}
