//! Reference index for efficient symbol usage lookups.
//!
//! This module provides:
//! - O(1) lookup of all references to a symbol by ID
//! - Per-symbol reference lists for quick find-all-references
//! - Support for incremental updates when files change
//! - Reference filtering by kind (read, write, call, type_ref, import)

const std = @import("std");
const Allocator = std.mem.Allocator;

const symbols = @import("symbols.zig");
const SymbolId = symbols.SymbolId;
const SymbolReference = symbols.SymbolReference;
const SymbolTable = symbols.SymbolTable;
const invalid_symbol = symbols.invalid_symbol;

/// Index for efficient reference lookups.
///
/// Maintains a mapping from symbol ID to all reference indices,
/// enabling O(1) lookup of references to any symbol.
pub const ReferenceIndex = struct {
    allocator: Allocator,

    /// Maps symbol ID to indices into the reference list.
    /// Each entry is a list of indices into SymbolTable.references.
    symbol_to_refs: std.AutoHashMapUnmanaged(SymbolId, RefList),

    /// Compact list of reference indices per symbol.
    const RefList = struct {
        indices: std.ArrayListUnmanaged(usize),

        fn init() RefList {
            return .{ .indices = .{} };
        }

        fn deinit(self: *RefList, allocator: Allocator) void {
            self.indices.deinit(allocator);
        }

        fn append(self: *RefList, allocator: Allocator, index: usize) !void {
            try self.indices.append(allocator, index);
        }

        fn count(self: *const RefList) usize {
            return self.indices.items.len;
        }
    };

    pub fn init(allocator: Allocator) ReferenceIndex {
        return .{
            .allocator = allocator,
            .symbol_to_refs = .{},
        };
    }

    pub fn deinit(self: *ReferenceIndex) void {
        var it = self.symbol_to_refs.valueIterator();
        while (it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.symbol_to_refs.deinit(self.allocator);
    }

    /// Build the reference index from a symbol table.
    ///
    /// This scans all references once and builds the per-symbol index.
    pub fn buildFromSymbolTable(self: *ReferenceIndex, table: *const SymbolTable) !void {
        // Clear existing index.
        self.clear();

        // Index all references.
        for (table.references.items, 0..) |ref, i| {
            try self.addReferenceIndex(ref.symbol_id, i);
        }
    }

    /// Clear all indexed data.
    pub fn clear(self: *ReferenceIndex) void {
        var it = self.symbol_to_refs.valueIterator();
        while (it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.symbol_to_refs.clearRetainingCapacity();
    }

    /// Add a reference index for a symbol.
    pub fn addReferenceIndex(self: *ReferenceIndex, symbol_id: SymbolId, ref_index: usize) !void {
        const result = try self.symbol_to_refs.getOrPut(self.allocator, symbol_id);
        if (!result.found_existing) {
            result.value_ptr.* = RefList.init();
        }
        try result.value_ptr.append(self.allocator, ref_index);
    }

    /// Get all reference indices for a symbol.
    ///
    /// Returns indices into SymbolTable.references for all usages of the symbol.
    pub fn getReferenceIndices(self: *const ReferenceIndex, symbol_id: SymbolId) []const usize {
        if (self.symbol_to_refs.get(symbol_id)) |list| {
            return list.indices.items;
        }
        return &.{};
    }

    /// Get the count of references to a symbol.
    pub fn getReferenceCount(self: *const ReferenceIndex, symbol_id: SymbolId) usize {
        if (self.symbol_to_refs.get(symbol_id)) |list| {
            return list.count();
        }
        return 0;
    }

    /// Check if a symbol has any references.
    pub fn hasReferences(self: *const ReferenceIndex, symbol_id: SymbolId) bool {
        return self.getReferenceCount(symbol_id) > 0;
    }

    /// Get the number of indexed symbols.
    pub fn indexedSymbolCount(self: *const ReferenceIndex) usize {
        return self.symbol_to_refs.count();
    }

    // === Incremental Update Methods ===

    /// Remove all reference indices for a specific symbol.
    ///
    /// Use this when a symbol is deleted or redefined. After calling this,
    /// the caller should re-add any remaining valid references.
    pub fn removeSymbol(self: *ReferenceIndex, symbol_id: SymbolId) void {
        if (self.symbol_to_refs.fetchRemove(symbol_id)) |entry| {
            var list = entry.value;
            list.deinit(self.allocator);
        }
    }

    /// Remove references in a specific index range.
    ///
    /// Useful when re-analyzing a file: first remove references in the old
    /// range, then add the new references. Indices are invalidated after
    /// this call for that symbol; caller should rebuild the symbol's refs.
    pub fn removeReferencesInRange(
        self: *ReferenceIndex,
        symbol_id: SymbolId,
        min_index: usize,
        max_index: usize,
    ) void {
        if (self.symbol_to_refs.getPtr(symbol_id)) |list_ptr| {
            // Filter out indices in the removed range.
            var write_idx: usize = 0;
            for (list_ptr.indices.items) |ref_idx| {
                if (ref_idx < min_index or ref_idx >= max_index) {
                    list_ptr.indices.items[write_idx] = ref_idx;
                    write_idx += 1;
                }
            }
            list_ptr.indices.shrinkRetainingCapacity(write_idx);
        }
    }

    /// Update indices after references are removed from the table.
    ///
    /// When references are removed from SymbolTable.references, the indices
    /// in this index become stale. This method adjusts all indices greater
    /// than `removed_index` by decrementing them by `count`.
    pub fn shiftIndicesAfterRemoval(self: *ReferenceIndex, removed_index: usize, count: usize) void {
        var it = self.symbol_to_refs.valueIterator();
        while (it.next()) |list| {
            for (list.indices.items) |*idx| {
                if (idx.* >= removed_index + count) {
                    idx.* -= count;
                }
            }
        }
    }

    /// Incrementally rebuild references for a single file.
    ///
    /// This is the recommended approach for incremental updates:
    /// 1. Remove all references from the old file analysis
    /// 2. Re-analyze the file and add new references via addReferenceIndex
    ///
    /// For simplicity, this implementation removes all refs for affected
    /// symbols and re-adds them. A more sophisticated version could track
    /// file_id -> reference indices.
    pub fn rebuildForFile(
        self: *ReferenceIndex,
        table: *const SymbolTable,
        file_id: u32,
    ) !void {
        // Collect symbols with references in this file.
        var affected_symbols = std.AutoHashMapUnmanaged(SymbolId, void){};
        defer affected_symbols.deinit(self.allocator);

        for (table.references.items) |ref| {
            if (ref.span.file_id == file_id) {
                try affected_symbols.put(self.allocator, ref.symbol_id, {});
            }
        }

        // Remove and rebuild for each affected symbol.
        var it = affected_symbols.keyIterator();
        while (it.next()) |sym_id| {
            self.removeSymbol(sym_id.*);
        }

        // Re-add all references for affected symbols.
        for (table.references.items, 0..) |ref, i| {
            if (affected_symbols.contains(ref.symbol_id)) {
                try self.addReferenceIndex(ref.symbol_id, i);
            }
        }
    }
};

/// Result of a find-all-references query.
pub const ReferenceResult = struct {
    /// The symbol being referenced.
    symbol_id: SymbolId,
    /// All references to this symbol.
    references: []const ReferenceInfo,
    /// Whether to include the definition location.
    include_definition: bool,
    /// Definition info (if include_definition is true).
    definition: ?DefinitionInfo,

    pub const ReferenceInfo = struct {
        /// File containing this reference.
        file_id: u32,
        /// Line number (1-based).
        line: u32,
        /// Column number (1-based).
        column: u32,
        /// Byte offset in file.
        offset: usize,
        /// End offset (for range highlighting).
        end_offset: usize,
        /// Kind of reference.
        kind: SymbolReference.Kind,
        /// Context line (if available).
        context: ?[]const u8,
    };

    pub const DefinitionInfo = struct {
        file_id: u32,
        line: u32,
        column: u32,
        offset: usize,
    };
};

/// Find all references to a symbol.
///
/// Returns a ReferenceResult containing all usages of the symbol,
/// optionally including the definition location.
pub fn findReferences(
    allocator: Allocator,
    table: *const SymbolTable,
    index: *const ReferenceIndex,
    symbol_id: SymbolId,
    include_definition: bool,
) !ReferenceResult {
    const indices = index.getReferenceIndices(symbol_id);

    // Build reference info list.
    var refs = std.ArrayListUnmanaged(ReferenceResult.ReferenceInfo){};
    defer refs.deinit(allocator);

    for (indices) |ref_idx| {
        if (ref_idx < table.references.items.len) {
            const ref = table.references.items[ref_idx];
            try refs.append(allocator, .{
                .file_id = ref.span.file_id,
                .line = ref.span.start.line,
                .column = ref.span.start.column,
                .offset = ref.span.start.offset,
                .end_offset = ref.span.end.offset,
                .kind = ref.kind,
                .context = null, // Caller should provide source context
            });
        }
    }

    // Get definition info if requested.
    var def_info: ?ReferenceResult.DefinitionInfo = null;
    if (include_definition) {
        if (table.getSymbol(symbol_id)) |sym| {
            def_info = .{
                .file_id = sym.definition_span.file_id,
                .line = sym.definition_span.start.line,
                .column = sym.definition_span.start.column,
                .offset = sym.definition_span.start.offset,
            };
        }
    }

    // Transfer ownership.
    const owned_refs = try allocator.dupe(ReferenceResult.ReferenceInfo, refs.items);

    return .{
        .symbol_id = symbol_id,
        .references = owned_refs,
        .include_definition = include_definition,
        .definition = def_info,
    };
}

/// Free a ReferenceResult.
pub fn freeReferenceResult(allocator: Allocator, result: *ReferenceResult) void {
    allocator.free(result.references);
    result.references = &.{};
}

/// Filter references by kind.
pub fn filterReferencesByKind(
    allocator: Allocator,
    table: *const SymbolTable,
    index: *const ReferenceIndex,
    symbol_id: SymbolId,
    kind: SymbolReference.Kind,
) ![]const usize {
    const all_indices = index.getReferenceIndices(symbol_id);

    var filtered = std.ArrayListUnmanaged(usize){};
    defer filtered.deinit(allocator);

    for (all_indices) |ref_idx| {
        if (ref_idx < table.references.items.len) {
            const ref = table.references.items[ref_idx];
            if (ref.kind == kind) {
                try filtered.append(allocator, ref_idx);
            }
        }
    }

    return allocator.dupe(usize, filtered.items);
}

/// Get read references only.
pub fn getReadReferences(
    allocator: Allocator,
    table: *const SymbolTable,
    index: *const ReferenceIndex,
    symbol_id: SymbolId,
) ![]const usize {
    return filterReferencesByKind(allocator, table, index, symbol_id, .read);
}

/// Get write references only.
pub fn getWriteReferences(
    allocator: Allocator,
    table: *const SymbolTable,
    index: *const ReferenceIndex,
    symbol_id: SymbolId,
) ![]const usize {
    return filterReferencesByKind(allocator, table, index, symbol_id, .write);
}

/// Get call references only.
pub fn getCallReferences(
    allocator: Allocator,
    table: *const SymbolTable,
    index: *const ReferenceIndex,
    symbol_id: SymbolId,
) ![]const usize {
    return filterReferencesByKind(allocator, table, index, symbol_id, .call);
}

// === Tests ===

test "ReferenceIndex initialization" {
    var index = ReferenceIndex.init(std.testing.allocator);
    defer index.deinit();

    try std.testing.expectEqual(@as(usize, 0), index.indexedSymbolCount());
}

test "ReferenceIndex add and get references" {
    var index = ReferenceIndex.init(std.testing.allocator);
    defer index.deinit();

    // Add references for symbol 0.
    try index.addReferenceIndex(0, 0);
    try index.addReferenceIndex(0, 5);
    try index.addReferenceIndex(0, 10);

    // Add references for symbol 1.
    try index.addReferenceIndex(1, 2);
    try index.addReferenceIndex(1, 7);

    try std.testing.expectEqual(@as(usize, 2), index.indexedSymbolCount());

    // Check symbol 0 references.
    const refs_0 = index.getReferenceIndices(0);
    try std.testing.expectEqual(@as(usize, 3), refs_0.len);
    try std.testing.expectEqual(@as(usize, 0), refs_0[0]);
    try std.testing.expectEqual(@as(usize, 5), refs_0[1]);
    try std.testing.expectEqual(@as(usize, 10), refs_0[2]);

    // Check symbol 1 references.
    const refs_1 = index.getReferenceIndices(1);
    try std.testing.expectEqual(@as(usize, 2), refs_1.len);

    // Check non-existent symbol.
    const refs_99 = index.getReferenceIndices(99);
    try std.testing.expectEqual(@as(usize, 0), refs_99.len);
}

test "ReferenceIndex hasReferences" {
    var index = ReferenceIndex.init(std.testing.allocator);
    defer index.deinit();

    try index.addReferenceIndex(0, 0);

    try std.testing.expect(index.hasReferences(0));
    try std.testing.expect(!index.hasReferences(1));
}

test "ReferenceIndex clear" {
    var index = ReferenceIndex.init(std.testing.allocator);
    defer index.deinit();

    try index.addReferenceIndex(0, 0);
    try index.addReferenceIndex(1, 1);
    try std.testing.expectEqual(@as(usize, 2), index.indexedSymbolCount());

    index.clear();
    try std.testing.expectEqual(@as(usize, 0), index.indexedSymbolCount());
    try std.testing.expect(!index.hasReferences(0));
}

test "ReferenceIndex buildFromSymbolTable" {
    const utils = @import("utils");
    const Span = utils.Span;

    // Create a symbol table with some references.
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    _ = try table.pushScope(.global, Span.empty);

    // Add a symbol.
    const sym_id = try table.addSymbol(
        "test_var",
        .variable,
        0, // type_id
        Span.empty,
        0,
        false,
        false,
        invalid_symbol,
    );

    // Add references to it.
    try table.addReference(sym_id, Span.empty, 1, .read);
    try table.addReference(sym_id, Span.empty, 2, .read);
    try table.addReference(sym_id, Span.empty, 3, .write);

    // Build index.
    var index = ReferenceIndex.init(std.testing.allocator);
    defer index.deinit();

    try index.buildFromSymbolTable(&table);

    // Verify index.
    try std.testing.expectEqual(@as(usize, 3), index.getReferenceCount(sym_id));
    try std.testing.expect(index.hasReferences(sym_id));
}

test "findReferences with definition" {
    const utils = @import("utils");
    const Span = utils.Span;

    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    _ = try table.pushScope(.global, Span.empty);

    const def_span = Span{
        .start = .{ .line = 1, .column = 5, .offset = 4 },
        .end = .{ .line = 1, .column = 10, .offset = 9 },
        .file_id = 0,
    };

    const sym_id = try table.addSymbol(
        "my_func",
        .function,
        0,
        def_span,
        0,
        true,
        false,
        invalid_symbol,
    );

    const ref_span = Span{
        .start = .{ .line = 5, .column = 3, .offset = 50 },
        .end = .{ .line = 5, .column = 10, .offset = 57 },
        .file_id = 0,
    };
    try table.addReference(sym_id, ref_span, 10, .call);

    var index = ReferenceIndex.init(std.testing.allocator);
    defer index.deinit();
    try index.buildFromSymbolTable(&table);

    // Find references including definition.
    var result = try findReferences(std.testing.allocator, &table, &index, sym_id, true);
    defer freeReferenceResult(std.testing.allocator, &result);

    try std.testing.expectEqual(@as(usize, 1), result.references.len);
    try std.testing.expect(result.definition != null);
    try std.testing.expectEqual(@as(u32, 1), result.definition.?.line);
    try std.testing.expectEqual(@as(u32, 5), result.definition.?.column);

    // Check reference info.
    try std.testing.expectEqual(@as(u32, 5), result.references[0].line);
    try std.testing.expectEqual(SymbolReference.Kind.call, result.references[0].kind);
}

test "filterReferencesByKind" {
    const utils = @import("utils");
    const Span = utils.Span;

    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    _ = try table.pushScope(.global, Span.empty);

    const sym_id = try table.addSymbol(
        "counter",
        .mutable_variable,
        0,
        Span.empty,
        0,
        false,
        true,
        invalid_symbol,
    );

    // Add mixed references.
    try table.addReference(sym_id, Span.empty, 1, .read);
    try table.addReference(sym_id, Span.empty, 2, .write);
    try table.addReference(sym_id, Span.empty, 3, .read);
    try table.addReference(sym_id, Span.empty, 4, .read);

    var index = ReferenceIndex.init(std.testing.allocator);
    defer index.deinit();
    try index.buildFromSymbolTable(&table);

    // Filter by read.
    const reads = try getReadReferences(std.testing.allocator, &table, &index, sym_id);
    defer std.testing.allocator.free(reads);
    try std.testing.expectEqual(@as(usize, 3), reads.len);

    // Filter by write.
    const writes = try getWriteReferences(std.testing.allocator, &table, &index, sym_id);
    defer std.testing.allocator.free(writes);
    try std.testing.expectEqual(@as(usize, 1), writes.len);
}

test "ReferenceIndex removeSymbol" {
    var index = ReferenceIndex.init(std.testing.allocator);
    defer index.deinit();

    // Add references for two symbols.
    try index.addReferenceIndex(0, 0);
    try index.addReferenceIndex(0, 1);
    try index.addReferenceIndex(1, 2);

    try std.testing.expectEqual(@as(usize, 2), index.indexedSymbolCount());
    try std.testing.expect(index.hasReferences(0));

    // Remove symbol 0.
    index.removeSymbol(0);

    try std.testing.expectEqual(@as(usize, 1), index.indexedSymbolCount());
    try std.testing.expect(!index.hasReferences(0));
    try std.testing.expect(index.hasReferences(1));
}

test "ReferenceIndex removeReferencesInRange" {
    var index = ReferenceIndex.init(std.testing.allocator);
    defer index.deinit();

    // Add references at indices 0, 5, 10, 15, 20.
    try index.addReferenceIndex(0, 0);
    try index.addReferenceIndex(0, 5);
    try index.addReferenceIndex(0, 10);
    try index.addReferenceIndex(0, 15);
    try index.addReferenceIndex(0, 20);

    try std.testing.expectEqual(@as(usize, 5), index.getReferenceCount(0));

    // Remove indices in range [5, 16) - should remove 5, 10, 15.
    index.removeReferencesInRange(0, 5, 16);

    const refs = index.getReferenceIndices(0);
    try std.testing.expectEqual(@as(usize, 2), refs.len);
    try std.testing.expectEqual(@as(usize, 0), refs[0]);
    try std.testing.expectEqual(@as(usize, 20), refs[1]);
}

test "ReferenceIndex shiftIndicesAfterRemoval" {
    var index = ReferenceIndex.init(std.testing.allocator);
    defer index.deinit();

    // Add references at indices 0, 5, 10.
    try index.addReferenceIndex(0, 0);
    try index.addReferenceIndex(0, 5);
    try index.addReferenceIndex(0, 10);

    // Simulate removing 2 entries starting at index 3.
    // Indices >= 5 should decrease by 2.
    index.shiftIndicesAfterRemoval(3, 2);

    const refs = index.getReferenceIndices(0);
    try std.testing.expectEqual(@as(usize, 3), refs.len);
    try std.testing.expectEqual(@as(usize, 0), refs[0]); // Unchanged
    try std.testing.expectEqual(@as(usize, 3), refs[1]); // Was 5, now 5-2=3
    try std.testing.expectEqual(@as(usize, 8), refs[2]); // Was 10, now 10-2=8
}

test "ReferenceIndex rebuildForFile" {
    const utils = @import("utils");
    const Span = utils.Span;

    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    _ = try table.pushScope(.global, Span.empty);

    // Symbol in file 0.
    const sym_0 = try table.addSymbol(
        "var_a",
        .variable,
        0,
        Span.empty,
        0,
        false,
        false,
        invalid_symbol,
    );

    // Symbol in file 1.
    const span_file1 = Span{
        .start = .{ .line = 1, .column = 1, .offset = 0 },
        .end = .{ .line = 1, .column = 5, .offset = 4 },
        .file_id = 1,
    };
    const sym_1 = try table.addSymbol(
        "var_b",
        .variable,
        0,
        span_file1,
        1,
        false,
        false,
        invalid_symbol,
    );

    // Add references in different files.
    const ref_span_f0 = Span{
        .start = .{ .line = 2, .column = 1, .offset = 10 },
        .end = .{ .line = 2, .column = 5, .offset = 14 },
        .file_id = 0,
    };
    const ref_span_f1 = Span{
        .start = .{ .line = 3, .column = 1, .offset = 20 },
        .end = .{ .line = 3, .column = 5, .offset = 24 },
        .file_id = 1,
    };

    try table.addReference(sym_0, ref_span_f0, 2, .read);
    try table.addReference(sym_1, ref_span_f1, 3, .read);
    try table.addReference(sym_0, ref_span_f1, 4, .read); // sym_0 referenced from file 1

    // Build initial index.
    var index = ReferenceIndex.init(std.testing.allocator);
    defer index.deinit();
    try index.buildFromSymbolTable(&table);

    try std.testing.expectEqual(@as(usize, 2), index.getReferenceCount(sym_0));
    try std.testing.expectEqual(@as(usize, 1), index.getReferenceCount(sym_1));

    // Rebuild for file 1 only.
    try index.rebuildForFile(&table, 1);

    // Counts should remain the same (we're rebuilding from same data).
    try std.testing.expectEqual(@as(usize, 2), index.getReferenceCount(sym_0));
    try std.testing.expectEqual(@as(usize, 1), index.getReferenceCount(sym_1));
}
