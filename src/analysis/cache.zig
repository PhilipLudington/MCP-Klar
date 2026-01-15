//! Document cache for caching parsed and analyzed Klar files.
//!
//! The document cache stores parsed ASTs and type-checked results to avoid
//! redundant parsing and analysis when tools are invoked multiple times on
//! the same file. Cache entries are keyed by file content hash.

const std = @import("std");
const Allocator = std.mem.Allocator;
const parser_mod = @import("compiler").parser;
const checker_mod = @import("compiler").checker;
const ast_mod = @import("compiler").ast;

const Parser = parser_mod.Parser;
const Checker = checker_mod.Checker;
const Ast = ast_mod.Ast;

/// A cached document entry containing parsed AST and analysis results.
pub const CacheEntry = struct {
    /// The content hash used to validate cache freshness.
    content_hash: u64,

    /// The file path (owned).
    file_path: []const u8,

    /// The parsed AST (owned).
    ast: Ast,

    /// The type checker with all analysis results (owned).
    /// May be null if only parsing was done.
    checker: ?Checker,

    /// Timestamp when this entry was created.
    created_at: i64,

    /// Number of times this entry has been accessed.
    access_count: u64,

    pub fn deinit(self: *CacheEntry, allocator: Allocator) void {
        if (self.checker) |*c| {
            c.deinit();
        }
        self.ast.deinit();
        allocator.free(self.file_path);
        self.* = undefined;
    }
};

/// Statistics about cache usage.
pub const CacheStats = struct {
    /// Total number of cache hits.
    hits: u64,
    /// Total number of cache misses.
    misses: u64,
    /// Total number of entries currently in cache.
    entry_count: usize,
    /// Total number of entries evicted.
    evictions: u64,

    pub fn hitRate(self: CacheStats) f64 {
        const total = self.hits + self.misses;
        if (total == 0) return 0.0;
        return @as(f64, @floatFromInt(self.hits)) / @as(f64, @floatFromInt(total));
    }
};

/// Document cache for parsed and analyzed Klar files.
pub const DocumentCache = struct {
    allocator: Allocator,

    /// Cache entries keyed by content hash.
    entries: std.AutoHashMapUnmanaged(u64, CacheEntry),

    /// Maximum number of entries to keep in cache.
    max_entries: usize,

    /// Statistics about cache usage.
    stats: CacheStats,

    /// Default maximum number of cache entries.
    pub const default_max_entries: usize = 100;

    pub fn init(allocator: Allocator) DocumentCache {
        return initWithCapacity(allocator, default_max_entries);
    }

    pub fn initWithCapacity(allocator: Allocator, max_entries: usize) DocumentCache {
        return .{
            .allocator = allocator,
            .entries = .{},
            .max_entries = max_entries,
            .stats = .{
                .hits = 0,
                .misses = 0,
                .entry_count = 0,
                .evictions = 0,
            },
        };
    }

    pub fn deinit(self: *DocumentCache) void {
        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.entries.deinit(self.allocator);
        self.* = undefined;
    }

    /// Compute content hash for cache lookup.
    pub fn computeHash(content: []const u8) u64 {
        return std.hash.Wyhash.hash(0, content);
    }

    /// Get a cached entry if it exists and is valid.
    /// Returns null if entry doesn't exist or content has changed.
    pub fn get(self: *DocumentCache, file_path: []const u8, content: []const u8) ?*CacheEntry {
        const hash = computeHash(content);

        if (self.entries.getPtr(hash)) |entry| {
            // Verify the file path matches to handle hash collisions.
            if (std.mem.eql(u8, entry.file_path, file_path)) {
                entry.access_count += 1;
                self.stats.hits += 1;
                return entry;
            }
        }

        self.stats.misses += 1;
        return null;
    }

    /// Get a cached entry by hash only (for when content is already known).
    pub fn getByHash(self: *DocumentCache, hash: u64) ?*CacheEntry {
        if (self.entries.getPtr(hash)) |entry| {
            entry.access_count += 1;
            self.stats.hits += 1;
            return entry;
        }
        self.stats.misses += 1;
        return null;
    }

    /// Parse and cache a document.
    /// If the document is already cached, returns the cached entry.
    pub fn getOrParse(
        self: *DocumentCache,
        file_path: []const u8,
        content: []const u8,
    ) !*CacheEntry {
        const hash = computeHash(content);

        // Check if already cached.
        if (self.entries.getPtr(hash)) |entry| {
            if (std.mem.eql(u8, entry.file_path, file_path)) {
                entry.access_count += 1;
                self.stats.hits += 1;
                return entry;
            }
        }

        // Not cached - parse and add.
        self.stats.misses += 1;

        // Evict if at capacity.
        if (self.entries.count() >= self.max_entries) {
            try self.evictLRU();
        }

        // Parse the content.
        var p = Parser.init(self.allocator, content, 0);
        defer p.deinit();

        var ast = try p.parse();
        errdefer ast.deinit();

        // Create entry.
        const owned_path = try self.allocator.dupe(u8, file_path);
        errdefer self.allocator.free(owned_path);

        const entry = CacheEntry{
            .content_hash = hash,
            .file_path = owned_path,
            .ast = ast,
            .checker = null,
            .created_at = std.time.timestamp(),
            .access_count = 1,
        };

        try self.entries.put(self.allocator, hash, entry);
        self.stats.entry_count = self.entries.count();

        return self.entries.getPtr(hash).?;
    }

    /// Parse and type-check a document, caching the results.
    /// If the document is already fully cached, returns the cached entry.
    pub fn getOrAnalyze(
        self: *DocumentCache,
        file_path: []const u8,
        content: []const u8,
    ) !*CacheEntry {
        const hash = computeHash(content);

        // Check if already cached with analysis.
        if (self.entries.getPtr(hash)) |entry| {
            if (std.mem.eql(u8, entry.file_path, file_path) and entry.checker != null) {
                entry.access_count += 1;
                self.stats.hits += 1;
                return entry;
            }
        }

        // Either not cached or missing analysis.
        self.stats.misses += 1;

        // Check if we have a parse-only cache entry.
        if (self.entries.getPtr(hash)) |entry| {
            if (std.mem.eql(u8, entry.file_path, file_path) and entry.checker == null) {
                // Add analysis to existing entry.
                var checker = try Checker.init(self.allocator, &entry.ast);
                errdefer checker.deinit();

                try checker.check();

                entry.checker = checker;
                entry.access_count += 1;
                return entry;
            }
        }

        // Evict if at capacity.
        if (self.entries.count() >= self.max_entries) {
            try self.evictLRU();
        }

        // Parse the content.
        var p = Parser.init(self.allocator, content, 0);
        defer p.deinit();

        var ast = try p.parse();
        errdefer ast.deinit();

        // Type check.
        var checker = try Checker.init(self.allocator, &ast);
        errdefer checker.deinit();

        try checker.check();

        // Create entry.
        const owned_path = try self.allocator.dupe(u8, file_path);
        errdefer self.allocator.free(owned_path);

        const entry = CacheEntry{
            .content_hash = hash,
            .file_path = owned_path,
            .ast = ast,
            .checker = checker,
            .created_at = std.time.timestamp(),
            .access_count = 1,
        };

        try self.entries.put(self.allocator, hash, entry);
        self.stats.entry_count = self.entries.count();

        return self.entries.getPtr(hash).?;
    }

    /// Invalidate a cache entry by file path.
    pub fn invalidate(self: *DocumentCache, file_path: []const u8) void {
        var to_remove: ?u64 = null;

        var iter = self.entries.iterator();
        while (iter.next()) |kv| {
            if (std.mem.eql(u8, kv.value_ptr.file_path, file_path)) {
                to_remove = kv.key_ptr.*;
                break;
            }
        }

        if (to_remove) |hash| {
            if (self.entries.getPtr(hash)) |entry| {
                entry.deinit(self.allocator);
            }
            _ = self.entries.remove(hash);
            self.stats.entry_count = self.entries.count();
        }
    }

    /// Invalidate a cache entry by content hash.
    pub fn invalidateByHash(self: *DocumentCache, hash: u64) void {
        if (self.entries.getPtr(hash)) |entry| {
            entry.deinit(self.allocator);
        }
        _ = self.entries.remove(hash);
        self.stats.entry_count = self.entries.count();
    }

    /// Clear all cache entries.
    pub fn clear(self: *DocumentCache) void {
        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.entries.clearRetainingCapacity();
        self.stats.entry_count = 0;
    }

    /// Evict the least recently used entry.
    fn evictLRU(self: *DocumentCache) !void {
        var min_access: u64 = std.math.maxInt(u64);
        var oldest_time: i64 = std.math.maxInt(i64);
        var victim_hash: ?u64 = null;

        var iter = self.entries.iterator();
        while (iter.next()) |kv| {
            const entry = kv.value_ptr;
            // Prefer evicting entries with fewer accesses.
            // Tie-break by creation time.
            if (entry.access_count < min_access or
                (entry.access_count == min_access and entry.created_at < oldest_time))
            {
                min_access = entry.access_count;
                oldest_time = entry.created_at;
                victim_hash = kv.key_ptr.*;
            }
        }

        if (victim_hash) |hash| {
            if (self.entries.getPtr(hash)) |entry| {
                entry.deinit(self.allocator);
            }
            _ = self.entries.remove(hash);
            self.stats.evictions += 1;
            self.stats.entry_count = self.entries.count();
        }
    }

    /// Get cache statistics.
    pub fn getStats(self: *const DocumentCache) CacheStats {
        return self.stats;
    }

    /// Reset cache statistics.
    pub fn resetStats(self: *DocumentCache) void {
        self.stats = .{
            .hits = 0,
            .misses = 0,
            .entry_count = self.entries.count(),
            .evictions = 0,
        };
    }
};

// === Tests ===

test "DocumentCache init/deinit" {
    var cache = DocumentCache.init(std.testing.allocator);
    defer cache.deinit();

    try std.testing.expectEqual(@as(usize, 0), cache.stats.entry_count);
    try std.testing.expectEqual(@as(u64, 0), cache.stats.hits);
    try std.testing.expectEqual(@as(u64, 0), cache.stats.misses);
}

test "DocumentCache computeHash" {
    const hash1 = DocumentCache.computeHash("let x = 42");
    const hash2 = DocumentCache.computeHash("let x = 42");
    const hash3 = DocumentCache.computeHash("let x = 43");

    try std.testing.expectEqual(hash1, hash2);
    try std.testing.expect(hash1 != hash3);
}

test "DocumentCache getOrParse caches entry" {
    var cache = DocumentCache.init(std.testing.allocator);
    defer cache.deinit();

    const content = "let x = 42";

    // First call should be a miss.
    const entry1 = try cache.getOrParse("test.kl", content);
    try std.testing.expectEqual(@as(u64, 0), cache.stats.hits);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.misses);
    try std.testing.expectEqual(@as(usize, 1), cache.stats.entry_count);

    // Second call should be a hit.
    const entry2 = try cache.getOrParse("test.kl", content);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.hits);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.misses);

    // Should return the same entry.
    try std.testing.expectEqual(entry1, entry2);
    try std.testing.expectEqual(@as(u64, 2), entry2.access_count);
}

test "DocumentCache getOrAnalyze includes type checking" {
    var cache = DocumentCache.init(std.testing.allocator);
    defer cache.deinit();

    const content = "let x = 42";

    const entry = try cache.getOrAnalyze("test.kl", content);
    try std.testing.expect(entry.checker != null);
    try std.testing.expect(!entry.checker.?.hasErrors());
}

test "DocumentCache invalidate removes entry" {
    var cache = DocumentCache.init(std.testing.allocator);
    defer cache.deinit();

    const content = "let x = 42";

    _ = try cache.getOrParse("test.kl", content);
    try std.testing.expectEqual(@as(usize, 1), cache.stats.entry_count);

    cache.invalidate("test.kl");
    try std.testing.expectEqual(@as(usize, 0), cache.stats.entry_count);
}

test "DocumentCache clear removes all entries" {
    var cache = DocumentCache.init(std.testing.allocator);
    defer cache.deinit();

    _ = try cache.getOrParse("test1.kl", "let x = 1");
    _ = try cache.getOrParse("test2.kl", "let y = 2");
    try std.testing.expectEqual(@as(usize, 2), cache.stats.entry_count);

    cache.clear();
    try std.testing.expectEqual(@as(usize, 0), cache.stats.entry_count);
}

test "DocumentCache eviction on capacity" {
    var cache = DocumentCache.initWithCapacity(std.testing.allocator, 2);
    defer cache.deinit();

    _ = try cache.getOrParse("test1.kl", "let a = 1");
    _ = try cache.getOrParse("test2.kl", "let b = 2");
    try std.testing.expectEqual(@as(usize, 2), cache.stats.entry_count);

    // This should trigger eviction.
    _ = try cache.getOrParse("test3.kl", "let c = 3");
    try std.testing.expectEqual(@as(usize, 2), cache.stats.entry_count);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.evictions);
}

test "DocumentCache content change invalidates" {
    var cache = DocumentCache.init(std.testing.allocator);
    defer cache.deinit();

    const content1 = "let x = 42";
    const content2 = "let x = 43";

    _ = try cache.getOrParse("test.kl", content1);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.misses);

    // Different content should be a miss.
    _ = try cache.getOrParse("test.kl", content2);
    try std.testing.expectEqual(@as(u64, 2), cache.stats.misses);
    try std.testing.expectEqual(@as(usize, 2), cache.stats.entry_count);
}

test "DocumentCache hit rate calculation" {
    var stats = CacheStats{
        .hits = 75,
        .misses = 25,
        .entry_count = 10,
        .evictions = 5,
    };

    try std.testing.expectApproxEqAbs(@as(f64, 0.75), stats.hitRate(), 0.001);
}

test "DocumentCache hit rate with zero total" {
    var stats = CacheStats{
        .hits = 0,
        .misses = 0,
        .entry_count = 0,
        .evictions = 0,
    };

    try std.testing.expectApproxEqAbs(@as(f64, 0.0), stats.hitRate(), 0.001);
}

test "DocumentCache get returns null for unknown" {
    var cache = DocumentCache.init(std.testing.allocator);
    defer cache.deinit();

    const result = cache.get("nonexistent.kl", "some content");
    try std.testing.expect(result == null);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.misses);
}

test "DocumentCache analysis upgrade from parse-only" {
    var cache = DocumentCache.init(std.testing.allocator);
    defer cache.deinit();

    const content = "let x = 42";

    // First get parse-only.
    const entry1 = try cache.getOrParse("test.kl", content);
    try std.testing.expect(entry1.checker == null);

    // Now get with analysis - should upgrade the entry.
    const entry2 = try cache.getOrAnalyze("test.kl", content);
    try std.testing.expect(entry2.checker != null);

    // Should be the same entry upgraded.
    try std.testing.expectEqual(entry1, entry2);
}
