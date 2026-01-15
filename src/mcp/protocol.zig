//! JSON-RPC 2.0 protocol types for MCP.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// JSON-RPC 2.0 request.
pub const Request = struct {
    jsonrpc: []const u8 = "2.0",
    id: ?RequestId = null,
    method: []const u8,
    params: ?std.json.Value = null,

    pub const RequestId = union(enum) {
        integer: i64,
        string: []const u8,
    };
};

/// JSON-RPC 2.0 response.
pub const Response = struct {
    jsonrpc: []const u8 = "2.0",
    id: ?Request.RequestId = null,
    result: ?std.json.Value = null,
    @"error": ?Error = null,

    pub const Error = struct {
        code: i64,
        message: []const u8,
        data: ?std.json.Value = null,
    };
};

/// Standard JSON-RPC error codes.
pub const ErrorCode = struct {
    pub const parse_error: i64 = -32700;
    pub const invalid_request: i64 = -32600;
    pub const method_not_found: i64 = -32601;
    pub const invalid_params: i64 = -32602;
    pub const internal_error: i64 = -32603;
};

/// MCP server capabilities.
pub const ServerCapabilities = struct {
    tools: ?ToolsCapability = null,
    resources: ?ResourcesCapability = null,

    pub const ToolsCapability = struct {
        listChanged: bool = false,
    };

    pub const ResourcesCapability = struct {
        subscribe: bool = false,
        listChanged: bool = false,
    };
};

/// MCP server information.
pub const ServerInfo = struct {
    name: []const u8,
    version: []const u8,
};

/// MCP tool definition.
pub const Tool = struct {
    name: []const u8,
    description: []const u8,
    inputSchema: std.json.Value,
};

/// MCP content block.
pub const Content = struct {
    type: []const u8,
    text: []const u8,
};

/// Creates a success response.
pub fn successResponse(id: ?Request.RequestId, result: std.json.Value) Response {
    return .{
        .id = id,
        .result = result,
    };
}

/// Creates an error response.
pub fn errorResponse(id: ?Request.RequestId, code: i64, message: []const u8) Response {
    return .{
        .id = id,
        .@"error" = .{
            .code = code,
            .message = message,
        },
    };
}

test "Response creation" {
    const success = successResponse(.{ .integer = 1 }, .{ .bool = true });
    try std.testing.expect(success.result != null);
    try std.testing.expect(success.@"error" == null);

    const err = errorResponse(.{ .integer = 2 }, ErrorCode.method_not_found, "Method not found");
    try std.testing.expect(err.result == null);
    try std.testing.expect(err.@"error" != null);
    try std.testing.expectEqual(ErrorCode.method_not_found, err.@"error".?.code);
}
