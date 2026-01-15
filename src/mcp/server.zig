//! MCP server main loop.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("protocol.zig");
const transport = @import("transport.zig");
const router_mod = @import("router.zig");
const tools = @import("tools/root.zig");
const resources = @import("resources/root.zig");
const build_options = @import("build_options");
const config = @import("config");
const utils = @import("utils");
const analysis = @import("analysis");

const DocumentCache = analysis.DocumentCache;
const RuntimeConfig = config.RuntimeConfig;

const log = std.log.scoped(.server);

/// MCP Server for Klar language.
pub const Server = struct {
    allocator: Allocator,
    transport: transport.StdioTransport,
    router: router_mod.Router,
    running: bool,

    /// Document cache for parsed and analyzed files.
    cache: DocumentCache,

    /// Runtime configuration.
    runtime_config: *const RuntimeConfig,

    pub fn init(allocator: Allocator, runtime_config: *const RuntimeConfig) !Server {
        var server = Server{
            .allocator = allocator,
            .transport = transport.StdioTransport.init(allocator),
            .router = router_mod.Router.init(allocator),
            .running = false,
            .cache = DocumentCache.init(allocator),
            .runtime_config = runtime_config,
        };

        // Register tool handlers
        try server.router.register("klar_check", tools.check.execute);
        try server.router.register("klar_completions", tools.completions.execute);
        try server.router.register("klar_definition", tools.definition.execute);
        try server.router.register("klar_hover", tools.hover.execute);
        try server.router.register("klar_references", tools.references.execute);
        try server.router.register("klar_symbols", tools.symbols.execute);

        // Register resource handlers
        try server.router.registerResource(.{
            .uri = resources.std_docs.uri,
            .name = resources.std_docs.name,
            .description = resources.std_docs.description,
            .mime_type = resources.std_docs.mime_type,
        }, resources.std_docs.execute);

        try server.router.registerResource(.{
            .uri = resources.project_structure.uri,
            .name = resources.project_structure.name,
            .description = resources.project_structure.description,
            .mime_type = resources.project_structure.mime_type,
        }, resources.project_structure.execute);

        return server;
    }

    pub fn deinit(self: *Server) void {
        self.cache.deinit();
        self.router.deinit();
        self.transport.deinit();
    }

    /// Get the document cache for use by tools.
    pub fn getCache(self: *Server) *DocumentCache {
        return &self.cache;
    }

    /// Get cache statistics for monitoring.
    pub fn getCacheStats(self: *const Server) analysis.CacheStats {
        return self.cache.getStats();
    }

    /// Main server loop.
    /// This loop is designed to be robust - individual message errors
    /// are logged but don't crash the server.
    pub fn run(self: *Server) !void {
        self.running = true;

        if (build_options.enable_logging or self.runtime_config.verbose) {
            log.info("Server running, waiting for messages...", .{});
        }

        while (self.running) {
            // Read message - EOF is handled gracefully
            const message = self.transport.readMessage() catch |err| {
                // Transport error - log and continue if possible
                log.err("Transport read error: {s}", .{@errorName(err)});
                if (err == error.EndOfStream or err == error.BrokenPipe) {
                    break;
                }
                continue;
            };

            if (message == null) {
                // EOF - clean shutdown
                break;
            }

            // Handle message - errors are caught and don't crash the server
            self.handleMessage(message.?) catch |err| {
                log.err("Message handling error: {s}", .{@errorName(err)});
                // Try to send an error response, but don't fail the loop if we can't
                self.sendErrorResponse("Internal server error") catch |send_err| {
                    log.err("Failed to send error response: {s}", .{@errorName(send_err)});
                };
            };
        }

        if (build_options.enable_logging or self.runtime_config.verbose) {
            log.info("Server shutting down", .{});
        }
    }

    /// Send a generic error response (used for internal errors).
    fn sendErrorResponse(self: *Server, message: []const u8) !void {
        const response = protocol.errorResponse(null, protocol.ErrorCode.internal_error, message);
        try self.sendResponse(response);
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
    var runtime_config = try RuntimeConfig.init(std.testing.allocator);
    defer runtime_config.deinit();

    var server = try Server.init(std.testing.allocator, &runtime_config);
    defer server.deinit();
}
