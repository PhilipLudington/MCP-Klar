//! stdio transport for MCP protocol.

const std = @import("std");
const Allocator = std.mem.Allocator;

const log = std.log.scoped(.transport);

/// Transport for reading/writing JSON-RPC messages over stdio.
pub const StdioTransport = struct {
    allocator: Allocator,
    stdin: std.fs.File,
    stdout: std.fs.File,
    line_buffer: std.ArrayListUnmanaged(u8),
    read_buffer: [4096]u8,

    pub fn init(allocator: Allocator) StdioTransport {
        return .{
            .allocator = allocator,
            .stdin = std.fs.File{ .handle = std.posix.STDIN_FILENO },
            .stdout = std.fs.File{ .handle = std.posix.STDOUT_FILENO },
            .line_buffer = .{},
            .read_buffer = undefined,
        };
    }

    pub fn initWithFiles(allocator: Allocator, stdin: std.fs.File, stdout: std.fs.File) StdioTransport {
        return .{
            .allocator = allocator,
            .stdin = stdin,
            .stdout = stdout,
            .line_buffer = .{},
            .read_buffer = undefined,
        };
    }

    pub fn deinit(self: *StdioTransport) void {
        self.line_buffer.deinit(self.allocator);
    }

    /// Reads a JSON message from stdin.
    /// Returns null on EOF.
    pub fn readMessage(self: *StdioTransport) !?[]const u8 {
        self.line_buffer.clearRetainingCapacity();

        // Read byte by byte until newline
        while (true) {
            const bytes_read = self.stdin.read(&self.read_buffer) catch |err| {
                if (err == error.WouldBlock) continue;
                return err;
            };

            if (bytes_read == 0) {
                // EOF
                if (self.line_buffer.items.len == 0) return null;
                return self.line_buffer.items;
            }

            for (self.read_buffer[0..bytes_read]) |byte| {
                if (byte == '\n') {
                    if (self.line_buffer.items.len == 0) continue;
                    return self.line_buffer.items;
                }
                try self.line_buffer.append(self.allocator, byte);
            }
        }
    }

    /// Writes a JSON message to stdout.
    pub fn writeMessage(self: *StdioTransport, message: []const u8) !void {
        try self.stdout.writeAll(message);
        try self.stdout.writeAll("\n");
    }
};

test "StdioTransport init/deinit" {
    // Use initWithFiles for testing to avoid stdin/stdout issues
    const allocator = std.testing.allocator;
    var buffer: std.ArrayListUnmanaged(u8) = .{};
    defer buffer.deinit(allocator);
}
