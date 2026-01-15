//! JSON serialization utilities for MCP protocol.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// JSON value type.
pub const Value = std.json.Value;

/// Parses a JSON string into a Value.
pub fn parse(allocator: Allocator, input: []const u8) !std.json.Parsed(Value) {
    return std.json.parseFromSlice(Value, allocator, input, .{});
}

/// Serializes a JSON Value to a string.
/// Caller owns returned memory.
pub fn stringifyValue(allocator: Allocator, value: Value) ![]u8 {
    var list = std.ArrayListUnmanaged(u8){};
    errdefer list.deinit(allocator);
    try writeValue(allocator, &list, value);
    return list.toOwnedSlice(allocator);
}

/// Writes a JSON Value to an ArrayList.
fn writeValue(allocator: Allocator, list: *std.ArrayListUnmanaged(u8), val: Value) Allocator.Error!void {
    switch (val) {
        .null => try list.appendSlice(allocator, "null"),
        .bool => |b| try list.appendSlice(allocator, if (b) "true" else "false"),
        .integer => |i| {
            var buf: [32]u8 = undefined;
            const s = std.fmt.bufPrint(&buf, "{d}", .{i}) catch unreachable;
            try list.appendSlice(allocator, s);
        },
        .float => |f| {
            var buf: [64]u8 = undefined;
            const s = std.fmt.bufPrint(&buf, "{d}", .{f}) catch unreachable;
            try list.appendSlice(allocator, s);
        },
        .string => |s| {
            try list.append(allocator, '"');
            try writeEscapedString(allocator, list, s);
            try list.append(allocator, '"');
        },
        .array => |arr| {
            try list.append(allocator, '[');
            for (arr.items, 0..) |item, i| {
                if (i > 0) try list.append(allocator, ',');
                try writeValue(allocator, list, item);
            }
            try list.append(allocator, ']');
        },
        .object => |obj| {
            try list.append(allocator, '{');
            var first = true;
            var it = obj.iterator();
            while (it.next()) |entry| {
                if (!first) try list.append(allocator, ',');
                first = false;
                try list.append(allocator, '"');
                try writeEscapedString(allocator, list, entry.key_ptr.*);
                try list.append(allocator, '"');
                try list.append(allocator, ':');
                try writeValue(allocator, list, entry.value_ptr.*);
            }
            try list.append(allocator, '}');
        },
        .number_string => |s| try list.appendSlice(allocator, s),
    }
}

/// Writes a string with JSON escaping.
fn writeEscapedString(allocator: Allocator, list: *std.ArrayListUnmanaged(u8), s: []const u8) Allocator.Error!void {
    for (s) |c| {
        switch (c) {
            '"' => try list.appendSlice(allocator, "\\\""),
            '\\' => try list.appendSlice(allocator, "\\\\"),
            '\n' => try list.appendSlice(allocator, "\\n"),
            '\r' => try list.appendSlice(allocator, "\\r"),
            '\t' => try list.appendSlice(allocator, "\\t"),
            0x00...0x08, 0x0B, 0x0C, 0x0E...0x1F => {
                var buf: [6]u8 = undefined;
                const hex = std.fmt.bufPrint(&buf, "\\u{X:0>4}", .{c}) catch unreachable;
                try list.appendSlice(allocator, hex);
            },
            else => try list.append(allocator, c),
        }
    }
}

/// A helper for building JSON objects.
pub const ObjectBuilder = struct {
    allocator: Allocator,
    map: std.json.ObjectMap,

    pub fn init(allocator: Allocator) ObjectBuilder {
        return .{
            .allocator = allocator,
            .map = std.json.ObjectMap.init(allocator),
        };
    }

    pub fn deinit(self: *ObjectBuilder) void {
        self.map.deinit();
    }

    pub fn put(self: *ObjectBuilder, key: []const u8, value: Value) !void {
        try self.map.put(key, value);
    }

    pub fn putString(self: *ObjectBuilder, key: []const u8, value: []const u8) !void {
        try self.map.put(key, .{ .string = value });
    }

    pub fn putInt(self: *ObjectBuilder, key: []const u8, value: i64) !void {
        try self.map.put(key, .{ .integer = value });
    }

    pub fn putBool(self: *ObjectBuilder, key: []const u8, value: bool) !void {
        try self.map.put(key, .{ .bool = value });
    }

    pub fn putNull(self: *ObjectBuilder, key: []const u8) !void {
        try self.map.put(key, .null);
    }

    pub fn build(self: *ObjectBuilder) Value {
        return .{ .object = self.map };
    }
};

/// A helper for building JSON arrays.
pub const ArrayBuilder = struct {
    allocator: Allocator,
    array: std.json.Array,

    pub fn init(allocator: Allocator) ArrayBuilder {
        return .{
            .allocator = allocator,
            .array = std.json.Array.init(allocator),
        };
    }

    pub fn deinit(self: *ArrayBuilder) void {
        self.array.deinit();
    }

    pub fn append(self: *ArrayBuilder, value: Value) !void {
        try self.array.append(value);
    }

    pub fn appendString(self: *ArrayBuilder, value: []const u8) !void {
        try self.array.append(.{ .string = value });
    }

    pub fn appendInt(self: *ArrayBuilder, value: i64) !void {
        try self.array.append(.{ .integer = value });
    }

    pub fn build(self: *ArrayBuilder) Value {
        return .{ .array = self.array };
    }
};

test "parse" {
    const allocator = std.testing.allocator;

    const input = "{\"name\":\"test\",\"value\":42}";
    var parsed = try parse(allocator, input);
    defer parsed.deinit();

    try std.testing.expectEqualStrings("test", parsed.value.object.get("name").?.string);
    try std.testing.expectEqual(@as(i64, 42), parsed.value.object.get("value").?.integer);
}

test "stringifyValue" {
    const allocator = std.testing.allocator;

    var obj = std.json.ObjectMap.init(allocator);
    defer obj.deinit();
    try obj.put("test", .{ .bool = true });

    const str = try stringifyValue(allocator, .{ .object = obj });
    defer allocator.free(str);

    try std.testing.expectEqualStrings("{\"test\":true}", str);
}

test "ObjectBuilder" {
    const allocator = std.testing.allocator;

    var builder = ObjectBuilder.init(allocator);
    defer builder.deinit();

    try builder.putString("name", "test");
    try builder.putInt("value", 42);
    try builder.putBool("active", true);

    const obj = builder.build();
    try std.testing.expectEqualStrings("test", obj.object.get("name").?.string);
    try std.testing.expectEqual(@as(i64, 42), obj.object.get("value").?.integer);
    try std.testing.expect(obj.object.get("active").?.bool);
}
