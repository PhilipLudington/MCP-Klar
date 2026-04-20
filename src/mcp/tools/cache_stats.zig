//! klar_cache_stats tool - Get cache performance statistics.
//!
//! This internal tool provides statistics about the document cache,
//! useful for debugging and performance monitoring.

const std = @import("std");
const Allocator = std.mem.Allocator;

const log = std.log.scoped(.klar_cache_stats);

/// Cache statistics result.
pub const CacheStatsResult = struct {
    hits: u64,
    misses: u64,
    hit_rate: f64,
    entry_count: usize,
    evictions: u64,
};

/// Execute the klar_cache_stats tool.
/// Note: This is a simple diagnostic tool that doesn't use the global cache
/// since tools don't have access to it directly. In a full implementation,
/// this would be integrated with the server to report actual statistics.
pub fn execute(allocator: Allocator, params: ?std.json.Value) !std.json.Value {
    _ = params;

    // Build response explaining that cache stats require server integration.
    var content_arr = std.json.Array.init(allocator);
    var content_obj = @as(std.json.ObjectMap, .empty);
    try content_obj.put(allocator, "type", .{ .string = "text" });
    try content_obj.put(allocator, "text", .{ .string = "Cache statistics are tracked at the server level. Use the server's getCacheStats() method for actual statistics." });
    try content_arr.append(.{ .object = content_obj });

    var outer = @as(std.json.ObjectMap, .empty);
    try outer.put(allocator, "content", .{ .array = content_arr });
    try outer.put(allocator, "isError", .{ .bool = false });

    // Add placeholder stats data.
    var data_obj = @as(std.json.ObjectMap, .empty);
    try data_obj.put(allocator, "note", .{ .string = "Cache statistics require server-level integration" });
    try data_obj.put(allocator, "cache_enabled", .{ .bool = true });
    try outer.put(allocator, "data", .{ .object = data_obj });

    return .{ .object = outer };
}

// === Tests ===

test "execute returns info response" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = try execute(allocator, null);

    const obj = result.object;
    const is_error = obj.get("isError").?.bool;
    try std.testing.expect(!is_error);
}
