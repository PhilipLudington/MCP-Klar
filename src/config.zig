//! Runtime configuration for Klar MCP Server.
//!
//! This module handles:
//! - Environment variable configuration (KLAR_STD_PATH, etc.)
//! - Project configuration file (klar.json) parsing
//!
//! Configuration precedence (highest to lowest):
//! 1. Environment variables
//! 2. Project klar.json file
//! 3. Default values

const std = @import("std");
const Allocator = std.mem.Allocator;

const log = std.log.scoped(.config);

/// Runtime configuration for the Klar MCP server.
pub const RuntimeConfig = struct {
    allocator: Allocator,

    /// Path to the Klar standard library.
    /// Set via KLAR_STD_PATH environment variable or klar.json.
    std_path: ?[]const u8,

    /// Project root directory (where klar.json was found, or cwd).
    project_root: []const u8,

    /// Source directories to search for Klar files.
    source_dirs: []const []const u8,

    /// Whether to enable verbose output.
    verbose: bool,

    /// Allocations that need to be freed on deinit.
    owned_strings: std.ArrayListUnmanaged([]const u8),

    /// Initialize configuration by loading from environment and klar.json.
    pub fn init(allocator: Allocator) !RuntimeConfig {
        var config = RuntimeConfig{
            .allocator = allocator,
            .std_path = null,
            .project_root = ".",
            .source_dirs = &.{},
            .verbose = false,
            .owned_strings = .{},
        };

        // First, try to load klar.json
        config.loadProjectConfig() catch |err| {
            // klar.json is optional, only log in debug builds
            if (@import("builtin").mode == .Debug) {
                log.debug("No klar.json found or failed to parse: {}", .{err});
            }
        };

        // Then, apply environment variable overrides
        config.loadEnvironmentOverrides();

        return config;
    }

    pub fn deinit(self: *RuntimeConfig) void {
        for (self.owned_strings.items) |s| {
            self.allocator.free(s);
        }
        self.owned_strings.deinit(self.allocator);
    }

    /// Load configuration from klar.json in the current directory or parent directories.
    fn loadProjectConfig(self: *RuntimeConfig) !void {
        const config_path = findConfigFile() orelse return error.ConfigNotFound;

        const file = try std.fs.cwd().openFile(config_path, .{});
        defer file.close();

        const content = try file.readToEndAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(content);

        try self.parseConfigJson(content);
    }

    /// Find klar.json in current directory or parent directories.
    fn findConfigFile() ?[]const u8 {
        // For now, just check current directory
        const config_path = "klar.json";
        if (std.fs.cwd().statFile(config_path)) |_| {
            return config_path;
        } else |_| {
            return null;
        }
    }

    /// Parse the JSON configuration content.
    /// Public for testing purposes.
    pub fn parseConfigJson(self: *RuntimeConfig, content: []const u8) !void {
        var parsed = try std.json.parseFromSlice(std.json.Value, self.allocator, content, .{});
        defer parsed.deinit();

        const root = parsed.value.object;

        // Parse std_path
        if (root.get("std_path")) |val| {
            if (val == .string) {
                const owned = try self.allocator.dupe(u8, val.string);
                try self.owned_strings.append(self.allocator, owned);
                self.std_path = owned;
            }
        }

        // Parse project_root
        if (root.get("project_root")) |val| {
            if (val == .string) {
                const owned = try self.allocator.dupe(u8, val.string);
                try self.owned_strings.append(self.allocator, owned);
                self.project_root = owned;
            }
        }

        // Parse verbose
        if (root.get("verbose")) |val| {
            if (val == .bool) {
                self.verbose = val.bool;
            }
        }

        // Parse source_dirs
        if (root.get("source_dirs")) |val| {
            if (val == .array) {
                var dirs = std.ArrayListUnmanaged([]const u8){};
                errdefer {
                    for (dirs.items) |d| self.allocator.free(d);
                    dirs.deinit(self.allocator);
                }

                for (val.array.items) |item| {
                    if (item == .string) {
                        const owned = try self.allocator.dupe(u8, item.string);
                        try dirs.append(self.allocator, owned);
                        try self.owned_strings.append(self.allocator, owned);
                    }
                }

                self.source_dirs = try dirs.toOwnedSlice(self.allocator);
            }
        }
    }

    /// Apply environment variable overrides.
    fn loadEnvironmentOverrides(self: *RuntimeConfig) void {
        // KLAR_STD_PATH overrides std_path from klar.json
        if (std.posix.getenv("KLAR_STD_PATH")) |path| {
            // Environment variable strings are static, no need to allocate
            self.std_path = path;
        }

        // KLAR_VERBOSE overrides verbose setting
        if (std.posix.getenv("KLAR_VERBOSE")) |val| {
            self.verbose = std.mem.eql(u8, val, "1") or
                std.mem.eql(u8, val, "true") or
                std.mem.eql(u8, val, "yes");
        }
    }

    /// Get the standard library path, returning null if not configured.
    pub fn getStdPath(self: *const RuntimeConfig) ?[]const u8 {
        return self.std_path;
    }

    /// Check if a standard library path is configured.
    pub fn hasStdPath(self: *const RuntimeConfig) bool {
        return self.std_path != null;
    }
};

/// Configuration file schema for klar.json.
/// This documents the expected format.
pub const ConfigSchema = struct {
    /// Path to the Klar standard library.
    std_path: ?[]const u8 = null,

    /// Project root directory.
    project_root: ?[]const u8 = null,

    /// Source directories to search for Klar files.
    source_dirs: ?[]const []const u8 = null,

    /// Enable verbose output.
    verbose: bool = false,
};

// === Tests ===

test "RuntimeConfig init with no klar.json" {
    var config = try RuntimeConfig.init(std.testing.allocator);
    defer config.deinit();

    // Should have sensible defaults
    try std.testing.expectEqualStrings(".", config.project_root);
    try std.testing.expectEqual(false, config.verbose);
}

test "RuntimeConfig parseConfigJson" {
    var cfg = RuntimeConfig{
        .allocator = std.testing.allocator,
        .std_path = null,
        .project_root = ".",
        .source_dirs = &.{},
        .verbose = false,
        .owned_strings = .{},
    };
    defer {
        // Free source_dirs slice if allocated
        if (cfg.source_dirs.len > 0) {
            std.testing.allocator.free(cfg.source_dirs);
        }
        cfg.deinit();
    }

    const json =
        \\{
        \\  "std_path": "/usr/local/klar/std",
        \\  "verbose": true,
        \\  "source_dirs": ["src", "lib"]
        \\}
    ;

    try cfg.parseConfigJson(json);

    try std.testing.expectEqualStrings("/usr/local/klar/std", cfg.std_path.?);
    try std.testing.expectEqual(true, cfg.verbose);
    try std.testing.expectEqual(@as(usize, 2), cfg.source_dirs.len);
    try std.testing.expectEqualStrings("src", cfg.source_dirs[0]);
    try std.testing.expectEqualStrings("lib", cfg.source_dirs[1]);
}

test "RuntimeConfig handles invalid json gracefully" {
    var cfg = RuntimeConfig{
        .allocator = std.testing.allocator,
        .std_path = null,
        .project_root = ".",
        .source_dirs = &.{},
        .verbose = false,
        .owned_strings = .{},
    };
    defer cfg.deinit();

    const result = cfg.parseConfigJson("not valid json");
    // JSON parser returns SyntaxError for invalid syntax
    try std.testing.expectError(error.SyntaxError, result);
}

test "RuntimeConfig handles empty json" {
    var config = RuntimeConfig{
        .allocator = std.testing.allocator,
        .std_path = null,
        .project_root = ".",
        .source_dirs = &.{},
        .verbose = false,
        .owned_strings = .{},
    };
    defer config.deinit();

    try config.parseConfigJson("{}");

    // Should keep defaults
    try std.testing.expectEqual(@as(?[]const u8, null), config.std_path);
    try std.testing.expectEqual(false, config.verbose);
}
