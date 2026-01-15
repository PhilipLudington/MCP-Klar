//! MCP server main loop.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("protocol.zig");
const transport = @import("transport.zig");
const router_mod = @import("router.zig");
const tools = @import("tools/root.zig");
const config = @import("config");
const utils = @import("utils");

const log = std.log.scoped(.server);

/// MCP Server for Klar language.
pub const Server = struct {
    allocator: Allocator,
    transport: transport.StdioTransport,
    router: router_mod.Router,
    running: bool,

    pub fn init(allocator: Allocator) !Server {
        var server = Server{
            .allocator = allocator,
            .transport = transport.StdioTransport.init(allocator),
            .router = router_mod.Router.init(allocator),
            .running = false,
        };

        // Register tool handlers
        try server.router.register("klar_check", tools.check.execute);

        return server;
    }

    pub fn deinit(self: *Server) void {
        self.router.deinit();
        self.transport.deinit();
    }

    /// Main server loop.
    pub fn run(self: *Server) !void {
        self.running = true;

        if (config.enable_logging) {
            log.info("Server running, waiting for messages...", .{});
        }

        while (self.running) {
            const message = try self.transport.readMessage();
            if (message == null) {
                // EOF
                break;
            }

            try self.handleMessage(message.?);
        }
    }

    fn handleMessage(self: *Server, message: []const u8) !void {
        // Parse JSON-RPC request
        var parsed = std.json.parseFromSlice(std.json.Value, self.allocator, message, .{}) catch {
            const response = protocol.errorResponse(null, protocol.ErrorCode.parse_error, "Parse error");
            try self.sendResponse(response);
            return;
        };
        defer parsed.deinit();

        const obj = parsed.value.object;

        // Extract request fields
        const method_value = obj.get("method") orelse {
            const response = protocol.errorResponse(null, protocol.ErrorCode.invalid_request, "Missing method");
            try self.sendResponse(response);
            return;
        };
        const method = method_value.string;

        const params = obj.get("params");

        // Get request ID
        var id: ?protocol.Request.RequestId = null;
        if (obj.get("id")) |id_value| {
            switch (id_value) {
                .integer => |i| id = .{ .integer = i },
                .string => |s| id = .{ .string = s },
                else => {},
            }
        }

        // Dispatch request
        var response = try self.router.dispatch(method, params);
        response.id = id;

        try self.sendResponse(response);
    }

    fn sendResponse(self: *Server, response: protocol.Response) !void {
        // Build JSON response manually
        var obj = std.json.ObjectMap.init(self.allocator);
        defer obj.deinit();

        try obj.put("jsonrpc", .{ .string = "2.0" });

        if (response.id) |id| {
            switch (id) {
                .integer => |i| try obj.put("id", .{ .integer = i }),
                .string => |s| try obj.put("id", .{ .string = s }),
            }
        } else {
            try obj.put("id", .null);
        }

        if (response.result) |result| {
            try obj.put("result", result);
        }

        if (response.@"error") |err| {
            var err_obj = std.json.ObjectMap.init(self.allocator);
            try err_obj.put("code", .{ .integer = err.code });
            try err_obj.put("message", .{ .string = err.message });
            try obj.put("error", .{ .object = err_obj });
        }

        const json_str = try utils.json.stringifyValue(self.allocator, .{ .object = obj });
        defer self.allocator.free(json_str);

        try self.transport.writeMessage(json_str);
    }
};

test "Server init/deinit" {
    var server = try Server.init(std.testing.allocator);
    defer server.deinit();
}
