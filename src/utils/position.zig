//! Position and span types for source code locations.

const std = @import("std");

/// A position in a source file (1-indexed line and column).
pub const Position = struct {
    /// 1-indexed line number.
    line: u32,
    /// 1-indexed column number (UTF-8 byte offset within line).
    column: u32,
    /// 0-indexed byte offset from start of file.
    offset: usize,

    pub const zero = Position{ .line = 0, .column = 0, .offset = 0 };

    /// Creates a position at the start of the file (line 1, column 1).
    pub fn start() Position {
        return .{ .line = 1, .column = 1, .offset = 0 };
    }

    /// Advances the position by one character.
    pub fn advance(self: Position, char: u8) Position {
        if (char == '\n') {
            return .{
                .line = self.line + 1,
                .column = 1,
                .offset = self.offset + 1,
            };
        }
        return .{
            .line = self.line,
            .column = self.column + 1,
            .offset = self.offset + 1,
        };
    }

    /// Formats the position as "line:column".
    pub fn format(
        self: Position,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{d}:{d}", .{ self.line, self.column });
    }
};

/// A span in a source file covering a range of positions.
pub const Span = struct {
    start: Position,
    end: Position,
    file_id: u32,

    pub const empty = Span{
        .start = Position.zero,
        .end = Position.zero,
        .file_id = 0,
    };

    /// Creates a span from a single position (zero-width).
    pub fn point(pos: Position, file_id: u32) Span {
        return .{
            .start = pos,
            .end = pos,
            .file_id = file_id,
        };
    }

    /// Merges two spans into one covering both.
    pub fn merge(a: Span, b: Span) Span {
        std.debug.assert(a.file_id == b.file_id);
        const start_pos = if (a.start.offset <= b.start.offset) a.start else b.start;
        const end_pos = if (a.end.offset >= b.end.offset) a.end else b.end;
        return .{
            .start = start_pos,
            .end = end_pos,
            .file_id = a.file_id,
        };
    }

    /// Checks if a position is within this span.
    pub fn contains(self: Span, pos: Position) bool {
        return pos.offset >= self.start.offset and pos.offset < self.end.offset;
    }

    /// Formats the span as "start-end".
    pub fn format(
        self: Span,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{}-{}", .{ self.start, self.end });
    }
};

test "Position.start" {
    const pos = Position.start();
    try std.testing.expectEqual(@as(u32, 1), pos.line);
    try std.testing.expectEqual(@as(u32, 1), pos.column);
    try std.testing.expectEqual(@as(usize, 0), pos.offset);
}

test "Position.advance" {
    var pos = Position.start();
    pos = pos.advance('a');
    try std.testing.expectEqual(@as(u32, 1), pos.line);
    try std.testing.expectEqual(@as(u32, 2), pos.column);
    try std.testing.expectEqual(@as(usize, 1), pos.offset);

    pos = pos.advance('\n');
    try std.testing.expectEqual(@as(u32, 2), pos.line);
    try std.testing.expectEqual(@as(u32, 1), pos.column);
    try std.testing.expectEqual(@as(usize, 2), pos.offset);
}

test "Span.merge" {
    const span1 = Span{
        .start = .{ .line = 1, .column = 1, .offset = 0 },
        .end = .{ .line = 1, .column = 5, .offset = 4 },
        .file_id = 0,
    };
    const span2 = Span{
        .start = .{ .line = 1, .column = 3, .offset = 2 },
        .end = .{ .line = 1, .column = 10, .offset = 9 },
        .file_id = 0,
    };
    const merged = Span.merge(span1, span2);
    try std.testing.expectEqual(@as(usize, 0), merged.start.offset);
    try std.testing.expectEqual(@as(usize, 9), merged.end.offset);
}

test "Span.contains" {
    const span = Span{
        .start = .{ .line = 1, .column = 1, .offset = 0 },
        .end = .{ .line = 1, .column = 5, .offset = 4 },
        .file_id = 0,
    };
    try std.testing.expect(span.contains(.{ .line = 1, .column = 2, .offset = 1 }));
    try std.testing.expect(!span.contains(.{ .line = 1, .column = 6, .offset = 5 }));
}
