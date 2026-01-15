//! Tool and resource dispatch router for MCP server.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("protocol.zig");

const Request = protocol.Request;
const Response = protocol.Response;
const ErrorCode = protocol.ErrorCode;

const log = std.log.scoped(.router);

/// Handler function type for tools.
pub const ToolHandler = *const fn (allocator: Allocator, params: ?std.json.Value) anyerror!std.json.Value;

/// Handler function type for resources.
/// Returns the content of the resource as a JSON value.
pub const ResourceHandler = *const fn (allocator: Allocator, uri: []const u8) anyerror!std.json.Value;

/// Resource definition for listing.
pub const ResourceDef = struct {
    uri: []const u8,
    name: []const u8,
    description: []const u8,
    mime_type: []const u8,
};

/// Router for dispatching tool and resource calls.
pub const Router = struct {
    allocator: Allocator,
    handlers: std.StringHashMapUnmanaged(ToolHandler),
    resource_handlers: std.StringHashMapUnmanaged(ResourceHandler),
    resource_defs: std.ArrayListUnmanaged(ResourceDef),

    pub fn init(allocator: Allocator) Router {
        return .{
            .allocator = allocator,
            .handlers = .{},
            .resource_handlers = .{},
            .resource_defs = .{},
        };
    }

    pub fn deinit(self: *Router) void {
        self.handlers.deinit(self.allocator);
        self.resource_handlers.deinit(self.allocator);
        self.resource_defs.deinit(self.allocator);
    }

    /// Registers a tool handler.
    pub fn register(self: *Router, name: []const u8, handler: ToolHandler) !void {
        try self.handlers.put(self.allocator, name, handler);
    }

    /// Registers a resource handler with its definition.
    pub fn registerResource(self: *Router, def: ResourceDef, handler: ResourceHandler) !void {
        try self.resource_handlers.put(self.allocator, def.uri, handler);
        try self.resource_defs.append(self.allocator, def);
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
        if (std.mem.eql(u8, method, "resources/list")) {
            return self.handleResourcesList();
        }
        if (std.mem.eql(u8, method, "resources/read")) {
            return self.handleResourcesRead(params);
        }

        return protocol.errorResponse(null, ErrorCode.method_not_found, "Unknown method");
    }

    fn handleInitialize(self: *Router, params: ?std.json.Value) Response {
        _ = params;

        var result = std.json.ObjectMap.init(self.allocator);

        // Build capabilities
        var capabilities = std.json.ObjectMap.init(self.allocator);

        // Tools capability
        var tools_cap = std.json.ObjectMap.init(self.allocator);
        tools_cap.put("listChanged", .{ .bool = false }) catch {};
        capabilities.put("tools", .{ .object = tools_cap }) catch {};

        // Resources capability
        var resources_cap = std.json.ObjectMap.init(self.allocator);
        resources_cap.put("subscribe", .{ .bool = false }) catch {};
        resources_cap.put("listChanged", .{ .bool = false }) catch {};
        capabilities.put("resources", .{ .object = resources_cap }) catch {};

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
        self.addTool(&tools, "klar_check", "Type check a Klar file and return diagnostics", &[_]ToolProperty{
            .{ .name = "file", .typ = "string", .description = "Path to .kl file", .required = true },
            .{ .name = "content", .typ = "string", .description = "File content override (for unsaved buffers)", .required = false },
        });

        // klar_completions tool
        self.addTool(&tools, "klar_completions", "Get code completions at a position in a Klar file", &[_]ToolProperty{
            .{ .name = "file", .typ = "string", .description = "Path to .kl file", .required = true },
            .{ .name = "line", .typ = "integer", .description = "Line number (1-indexed)", .required = true },
            .{ .name = "column", .typ = "integer", .description = "Column number (1-indexed)", .required = true },
            .{ .name = "content", .typ = "string", .description = "File content override (for unsaved buffers)", .required = false },
            .{ .name = "prefix", .typ = "string", .description = "Prefix to filter completions", .required = false },
        });

        // klar_definition tool
        self.addTool(&tools, "klar_definition", "Go to definition of a symbol at a position", &[_]ToolProperty{
            .{ .name = "file", .typ = "string", .description = "Path to .kl file", .required = true },
            .{ .name = "line", .typ = "integer", .description = "Line number (1-indexed)", .required = true },
            .{ .name = "column", .typ = "integer", .description = "Column number (1-indexed)", .required = true },
            .{ .name = "content", .typ = "string", .description = "File content override (for unsaved buffers)", .required = false },
        });

        // klar_hover tool
        self.addTool(&tools, "klar_hover", "Get type and documentation info for a symbol at a position", &[_]ToolProperty{
            .{ .name = "file", .typ = "string", .description = "Path to .kl file", .required = true },
            .{ .name = "line", .typ = "integer", .description = "Line number (1-indexed)", .required = true },
            .{ .name = "column", .typ = "integer", .description = "Column number (1-indexed)", .required = true },
            .{ .name = "content", .typ = "string", .description = "File content override (for unsaved buffers)", .required = false },
        });

        // klar_references tool
        self.addTool(&tools, "klar_references", "Find all references to a symbol at a position", &[_]ToolProperty{
            .{ .name = "file", .typ = "string", .description = "Path to .kl file", .required = true },
            .{ .name = "line", .typ = "integer", .description = "Line number (1-indexed)", .required = true },
            .{ .name = "column", .typ = "integer", .description = "Column number (1-indexed)", .required = true },
            .{ .name = "content", .typ = "string", .description = "File content override (for unsaved buffers)", .required = false },
            .{ .name = "include_definition", .typ = "boolean", .description = "Include the definition in results", .required = false },
        });

        // klar_symbols tool
        self.addTool(&tools, "klar_symbols", "List all symbols in a Klar file", &[_]ToolProperty{
            .{ .name = "file", .typ = "string", .description = "Path to .kl file", .required = true },
            .{ .name = "content", .typ = "string", .description = "File content override (for unsaved buffers)", .required = false },
            .{ .name = "kind", .typ = "string", .description = "Filter by symbol kind (function, struct, enum, variable, etc.)", .required = false },
        });

        var result = std.json.ObjectMap.init(self.allocator);
        result.put("tools", .{ .array = tools }) catch {};

        return protocol.successResponse(null, .{ .object = result });
    }

    const ToolProperty = struct {
        name: []const u8,
        typ: []const u8,
        description: []const u8,
        required: bool,
    };

    fn addTool(self: *Router, tools: *std.json.Array, name: []const u8, description: []const u8, properties: []const ToolProperty) void {
        var tool = std.json.ObjectMap.init(self.allocator);
        tool.put("name", .{ .string = name }) catch {};
        tool.put("description", .{ .string = description }) catch {};

        var schema = std.json.ObjectMap.init(self.allocator);
        schema.put("type", .{ .string = "object" }) catch {};

        var props = std.json.ObjectMap.init(self.allocator);
        var required_arr = std.json.Array.init(self.allocator);

        for (properties) |prop| {
            var prop_obj = std.json.ObjectMap.init(self.allocator);
            prop_obj.put("type", .{ .string = prop.typ }) catch {};
            prop_obj.put("description", .{ .string = prop.description }) catch {};
            props.put(prop.name, .{ .object = prop_obj }) catch {};

            if (prop.required) {
                required_arr.append(.{ .string = prop.name }) catch {};
            }
        }

        schema.put("properties", .{ .object = props }) catch {};
        schema.put("required", .{ .array = required_arr }) catch {};
        tool.put("inputSchema", .{ .object = schema }) catch {};

        tools.append(.{ .object = tool }) catch {};
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

    fn handleResourcesList(self: *Router) Response {
        var resources = std.json.Array.init(self.allocator);

        for (self.resource_defs.items) |def| {
            var resource = std.json.ObjectMap.init(self.allocator);
            resource.put("uri", .{ .string = def.uri }) catch {};
            resource.put("name", .{ .string = def.name }) catch {};
            resource.put("description", .{ .string = def.description }) catch {};
            resource.put("mimeType", .{ .string = def.mime_type }) catch {};
            resources.append(.{ .object = resource }) catch {};
        }

        var result = std.json.ObjectMap.init(self.allocator);
        result.put("resources", .{ .array = resources }) catch {};

        return protocol.successResponse(null, .{ .object = result });
    }

    fn handleResourcesRead(self: *Router, params: ?std.json.Value) Response {
        const p = params orelse {
            return protocol.errorResponse(null, ErrorCode.invalid_params, "Missing params");
        };

        const obj = p.object;
        const uri_value = obj.get("uri") orelse {
            return protocol.errorResponse(null, ErrorCode.invalid_params, "Missing resource URI");
        };
        const uri = switch (uri_value) {
            .string => |s| s,
            else => return protocol.errorResponse(null, ErrorCode.invalid_params, "URI must be a string"),
        };

        const handler = self.resource_handlers.get(uri) orelse {
            return protocol.errorResponse(null, ErrorCode.method_not_found, "Unknown resource");
        };

        const result = handler(self.allocator, uri) catch |err| {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Resource error: {s}", .{@errorName(err)}) catch "Resource error";
            return protocol.errorResponse(null, ErrorCode.internal_error, msg);
        };

        return protocol.successResponse(null, result);
    }
};

test "Router init/deinit" {
    var router = Router.init(std.testing.allocator);
    defer router.deinit();
}
