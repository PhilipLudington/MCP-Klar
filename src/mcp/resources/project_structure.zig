//! klar://project/structure resource - Project tree structure.
//!
//! This resource provides the project file structure, showing all Klar
//! source files and their organization within the project.

const std = @import("std");
const Allocator = std.mem.Allocator;

const log = std.log.scoped(.project_structure);

/// Resource URI for project structure.
pub const uri = "klar://project/structure";

/// Resource name for display.
pub const name = "Klar Project Structure";

/// Resource description.
pub const description = "Project file structure showing all Klar source files and their organization.";

/// MIME type for the resource.
pub const mime_type = "text/plain";

/// Maximum depth for directory traversal.
const max_depth: usize = 10;

/// Maximum number of files to include.
const max_files: usize = 1000;

/// Execute the resource read operation.
/// Returns the project structure as a JSON value.
pub fn execute(allocator: Allocator, requested_uri: []const u8) !std.json.Value {
    _ = requested_uri;

    var structure_builder = std.ArrayListUnmanaged(u8){};
    defer structure_builder.deinit(allocator);

    // Build the project structure
    try buildProjectStructure(allocator, &structure_builder);

    // Copy the content since we need to return it
    const structure_content = try allocator.dupe(u8, structure_builder.items);

    // Build MCP resource response
    var contents_arr = std.json.Array.init(allocator);

    var content_obj = std.json.ObjectMap.init(allocator);
    try content_obj.put("uri", .{ .string = uri });
    try content_obj.put("mimeType", .{ .string = mime_type });
    try content_obj.put("text", .{ .string = structure_content });
    try contents_arr.append(.{ .object = content_obj });

    var result = std.json.ObjectMap.init(allocator);
    try result.put("contents", .{ .array = contents_arr });

    return .{ .object = result };
}

/// Build the project structure string.
fn buildProjectStructure(allocator: Allocator, output: *std.ArrayListUnmanaged(u8)) !void {
    const writer = output.writer(allocator);

    try writer.writeAll("# Klar Project Structure\n\n");

    // Get current working directory
    const cwd = std.fs.cwd();

    // Check for klar.json to identify project root
    const has_klar_json = blk: {
        var file = cwd.openFile("klar.json", .{}) catch break :blk false;
        file.close();
        break :blk true;
    };

    if (has_klar_json) {
        try writer.writeAll("Project root: (klar.json found)\n\n");
    } else {
        try writer.writeAll("Project root: (current directory)\n\n");
    }

    try writer.writeAll("```\n");

    // Scan the directory tree
    var file_count: usize = 0;
    var dir_count: usize = 0;
    try scanDirectory(allocator, cwd, ".", output, 0, &file_count, &dir_count);

    try writer.writeAll("```\n\n");

    // Summary
    var buf: [128]u8 = undefined;
    const summary = std.fmt.bufPrint(&buf, "Total: {d} Klar files in {d} directories\n", .{ file_count, dir_count }) catch "Total: unknown";
    try writer.writeAll(summary);
}

/// Recursively scan a directory for Klar files.
fn scanDirectory(
    allocator: Allocator,
    dir: std.fs.Dir,
    path: []const u8,
    output: *std.ArrayListUnmanaged(u8),
    depth: usize,
    file_count: *usize,
    dir_count: *usize,
) !void {
    if (depth > max_depth) return;
    if (file_count.* >= max_files) return;

    const writer = output.writer(allocator);

    // Open the directory for iteration
    var iter_dir = dir.openDir(path, .{ .iterate = true }) catch |err| {
        // Skip directories we can't open
        log.debug("Cannot open directory {s}: {s}", .{ path, @errorName(err) });
        return;
    };
    defer iter_dir.close();

    // Collect entries to sort them
    var entries = std.ArrayListUnmanaged(Entry){};
    defer entries.deinit(allocator);

    var iter = iter_dir.iterate();
    while (iter.next() catch null) |entry| {
        // Skip hidden files and directories
        if (entry.name.len > 0 and entry.name[0] == '.') continue;

        // Skip common non-source directories
        if (std.mem.eql(u8, entry.name, "zig-out") or
            std.mem.eql(u8, entry.name, "zig-cache") or
            std.mem.eql(u8, entry.name, ".zig-cache") or
            std.mem.eql(u8, entry.name, "node_modules") or
            std.mem.eql(u8, entry.name, "target") or
            std.mem.eql(u8, entry.name, "build"))
        {
            continue;
        }

        const name_copy = try allocator.dupe(u8, entry.name);
        try entries.append(allocator, .{
            .name = name_copy,
            .kind = entry.kind,
        });
    }

    // Sort entries: directories first, then files, alphabetically
    std.mem.sort(Entry, entries.items, {}, entryLessThan);

    // Process directories first
    for (entries.items) |entry| {
        if (entry.kind == .directory) {
            // Print directory
            try writeIndent(writer, depth);
            try writer.print("{s}/\n", .{entry.name});
            dir_count.* += 1;

            // Recurse into directory
            try scanDirectory(allocator, iter_dir, entry.name, output, depth + 1, file_count, dir_count);
        }
    }

    // Then process files
    for (entries.items) |entry| {
        if (entry.kind == .file) {
            // Only show .kl files
            if (std.mem.endsWith(u8, entry.name, ".kl")) {
                try writeIndent(writer, depth);
                try writer.print("{s}\n", .{entry.name});
                file_count.* += 1;

                if (file_count.* >= max_files) return;
            }
        }
    }

    // Free entry names
    for (entries.items) |entry| {
        allocator.free(entry.name);
    }
}

const Entry = struct {
    name: []const u8,
    kind: std.fs.Dir.Entry.Kind,
};

fn entryLessThan(_: void, a: Entry, b: Entry) bool {
    // Directories come first
    if (a.kind == .directory and b.kind != .directory) return true;
    if (a.kind != .directory and b.kind == .directory) return false;

    // Then alphabetically
    return std.mem.lessThan(u8, a.name, b.name);
}

fn writeIndent(writer: anytype, depth: usize) !void {
    for (0..depth) |_| {
        try writer.writeAll("  ");
    }
}

// === Tests ===

test "execute returns valid response" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const result = try execute(arena.allocator(), uri);
    const obj = result.object;

    // Check that contents array exists
    const contents = obj.get("contents").?.array;
    try std.testing.expectEqual(@as(usize, 1), contents.items.len);

    // Check first content item
    const content = contents.items[0].object;
    try std.testing.expectEqualStrings(uri, content.get("uri").?.string);
    try std.testing.expectEqualStrings(mime_type, content.get("mimeType").?.string);

    // Check that text content exists
    const text = content.get("text").?.string;
    try std.testing.expect(text.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, text, "Klar Project Structure") != null);
}

test "writeIndent produces correct indentation" {
    var list = std.ArrayListUnmanaged(u8){};
    defer list.deinit(std.testing.allocator);

    const writer = list.writer(std.testing.allocator);

    try writeIndent(writer, 0);
    try std.testing.expectEqualStrings("", list.items);

    list.clearRetainingCapacity();
    try writeIndent(writer, 1);
    try std.testing.expectEqualStrings("  ", list.items);

    list.clearRetainingCapacity();
    try writeIndent(writer, 3);
    try std.testing.expectEqualStrings("      ", list.items);
}

test "entryLessThan sorts directories first" {
    const dir_entry = Entry{ .name = "zzz", .kind = .directory };
    const file_entry = Entry{ .name = "aaa", .kind = .file };

    // Directory should come before file regardless of name
    try std.testing.expect(entryLessThan({}, dir_entry, file_entry));
    try std.testing.expect(!entryLessThan({}, file_entry, dir_entry));
}

test "entryLessThan sorts alphabetically within same kind" {
    const file_a = Entry{ .name = "aaa.kl", .kind = .file };
    const file_b = Entry{ .name = "bbb.kl", .kind = .file };

    try std.testing.expect(entryLessThan({}, file_a, file_b));
    try std.testing.expect(!entryLessThan({}, file_b, file_a));
}
