//! Tool dispatch router for MCP server.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("protocol.zig");

const Request = protocol.Request;
const Response = protocol.Response;
const ErrorCode = protocol.ErrorCode;

const log = std.log.scoped(.router);

/// Handler function type for tools.
pub const ToolHandler = *const fn (allocator: Allocator, params: ?std.json.Value) anyerror!std.json.Value;

/// Router for dispatching tool calls.
pub const Router = struct {
    allocator: Allocator,
    handlers: std.StringHashMapUnmanaged(ToolHandler),

    pub fn init(allocator: Allocator) Router {
        return .{
            .allocator = allocator,
            .handlers = .{},
        };
    }

    pub fn deinit(self: *Router) void {
        self.handlers.deinit(self.allocator);
    }

    /// Registers a tool handler.
    pub fn register(self: *Router, name: []const u8, handler: ToolHandler) !void {
        try self.handlers.put(self.allocator, name, handler);
    }

    /// Dispatches a request to the appropriate handler.
    pub fn dispatch(self: *Router, method: []const u8, params: ?std.json.Value) !Response {
        // Handle MCP lifecycle methods
        if (std.mem.eql(u8, method, "initialize")) {
            return self.handleInitialize(params);
        }
        if (std.mem.eql(u8, method, "initialized")) {
            return self.handleInitialized();
        }
        if (std.mem.eql(u8, method, "tools/list")) {
            return self.handleToolsList();
        }
        if (std.mem.eql(u8, method, "tools/call")) {
            return self.handleToolsCall(params);
        }

        return protocol.errorResponse(null, ErrorCode.method_not_found, "Unknown method");
    }

    fn handleInitialize(self: *Router, params: ?std.json.Value) Response {
        _ = params;

        var result = std.json.ObjectMap.init(self.allocator);

        // Build capabilities
        var capabilities = std.json.ObjectMap.init(self.allocator);
        var tools_cap = std.json.ObjectMap.init(self.allocator);
        tools_cap.put("listChanged", .{ .bool = false }) catch {};
        capabilities.put("tools", .{ .object = tools_cap }) catch {};

        // Build server info
        var server_info = std.json.ObjectMap.init(self.allocator);
        server_info.put("name", .{ .string = "mcp-klar" }) catch {};
        server_info.put("version", .{ .string = "0.1.0" }) catch {};

        result.put("protocolVersion", .{ .string = "2024-11-05" }) catch {};
        result.put("capabilities", .{ .object = capabilities }) catch {};
        result.put("serverInfo", .{ .object = server_info }) catch {};

        return protocol.successResponse(null, .{ .object = result });
    }

    fn handleInitialized(self: *Router) Response {
        return protocol.successResponse(null, .{ .object = std.json.ObjectMap.init(self.allocator) });
    }

    fn handleToolsList(self: *Router) Response {
        var tools = std.json.Array.init(self.allocator);

        // klar_check tool
        var check_tool = std.json.ObjectMap.init(self.allocator);
        check_tool.put("name", .{ .string = "klar_check" }) catch {};
        check_tool.put("description", .{ .string = "Type check a Klar file and return diagnostics" }) catch {};

        var check_schema = std.json.ObjectMap.init(self.allocator);
        check_schema.put("type", .{ .string = "object" }) catch {};

        var check_props = std.json.ObjectMap.init(self.allocator);

        var file_prop = std.json.ObjectMap.init(self.allocator);
        file_prop.put("type", .{ .string = "string" }) catch {};
        file_prop.put("description", .{ .string = "Path to .kl file" }) catch {};
        check_props.put("file", .{ .object = file_prop }) catch {};

        var content_prop = std.json.ObjectMap.init(self.allocator);
        content_prop.put("type", .{ .string = "string" }) catch {};
        content_prop.put("description", .{ .string = "File content override (for unsaved buffers)" }) catch {};
        check_props.put("content", .{ .object = content_prop }) catch {};

        check_schema.put("properties", .{ .object = check_props }) catch {};

        var required = std.json.Array.init(self.allocator);
        required.append(.{ .string = "file" }) catch {};
        check_schema.put("required", .{ .array = required }) catch {};

        check_tool.put("inputSchema", .{ .object = check_schema }) catch {};
        tools.append(.{ .object = check_tool }) catch {};

        var result = std.json.ObjectMap.init(self.allocator);
        result.put("tools", .{ .array = tools }) catch {};

        return protocol.successResponse(null, .{ .object = result });
    }

    fn handleToolsCall(self: *Router, params: ?std.json.Value) Response {
        const p = params orelse {
            return protocol.errorResponse(null, ErrorCode.invalid_params, "Missing params");
        };

        const obj = p.object;
        const name_value = obj.get("name") orelse {
            return protocol.errorResponse(null, ErrorCode.invalid_params, "Missing tool name");
        };
        const name = name_value.string;

        const arguments = obj.get("arguments");

        const handler = self.handlers.get(name) orelse {
            return protocol.errorResponse(null, ErrorCode.method_not_found, "Unknown tool");
        };

        const result = handler(self.allocator, arguments) catch |err| {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Tool error: {s}", .{@errorName(err)}) catch "Tool error";
            return protocol.errorResponse(null, ErrorCode.internal_error, msg);
        };

        return protocol.successResponse(null, result);
    }
};

test "Router init/deinit" {
    var router = Router.init(std.testing.allocator);
    defer router.deinit();
}
