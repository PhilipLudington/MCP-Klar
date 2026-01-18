//! End-to-end integration tests for the MCP-Klar server.
//!
//! These tests verify the full MCP protocol communication flow,
//! from JSON-RPC requests to tool execution and response formatting.

const std = @import("std");
const testing = std.testing;

const mcp = @import("mcp");
const config = @import("config");
const utils = @import("utils");

const Server = mcp.Server;
const RuntimeConfig = config.RuntimeConfig;
const Router = mcp.router.Router;

// ============================================================================
// MCP Lifecycle Tests
// ============================================================================

test "initialize returns server capabilities" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var router = Router.init(alloc);
    defer router.deinit();

    const response = try router.dispatch("initialize", null);

    // Should have a result
    const result = response.result orelse return error.TestUnexpectedResult;
    const result_obj = result.object;

    // Check protocol version
    const proto_val = result_obj.get("protocolVersion") orelse return error.TestUnexpectedResult;
    try testing.expectEqualStrings("2024-11-05", proto_val.string);

    // Check server info
    const info_val = result_obj.get("serverInfo") orelse return error.TestUnexpectedResult;
    const info = info_val.object;
    const name_val = info.get("name") orelse return error.TestUnexpectedResult;
    try testing.expectEqualStrings("mcp-klar", name_val.string);
}

test "tools/list returns registered tools" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var router = Router.init(alloc);
    defer router.deinit();

    // Register some test handlers
    const dummy_handler = struct {
        fn handle(_: std.mem.Allocator, _: ?std.json.Value) anyerror!std.json.Value {
            return .null;
        }
    }.handle;
    try router.register("test_tool", dummy_handler);

    const response = try router.dispatch("tools/list", null);
    const result = response.result orelse return error.TestUnexpectedResult;
    const result_obj = result.object;
    const tools_val = result_obj.get("tools") orelse return error.TestUnexpectedResult;

    try testing.expect(tools_val == .array);
}

test "resources/list returns registered resources" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var router = Router.init(alloc);
    defer router.deinit();

    const response = try router.dispatch("resources/list", null);
    const result = response.result orelse return error.TestUnexpectedResult;
    const result_obj = result.object;
    const resources_val = result_obj.get("resources") orelse return error.TestUnexpectedResult;

    try testing.expect(resources_val == .array);
}

// ============================================================================
// Error Handling Tests
// ============================================================================

test "unknown method returns error" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var router = Router.init(alloc);
    defer router.deinit();

    const response = try router.dispatch("unknown/method", null);

    const err = response.@"error" orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(i64, -32601), err.code);
    try testing.expectEqualStrings("Unknown method", err.message);
}

test "tools/call with missing params returns error" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var router = Router.init(alloc);
    defer router.deinit();

    const response = try router.dispatch("tools/call", null);

    const err = response.@"error" orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(i64, -32602), err.code);
}

test "tools/call with unknown tool returns error" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var router = Router.init(alloc);
    defer router.deinit();

    var params = std.json.ObjectMap.init(alloc);
    try params.put("name", .{ .string = "unknown_tool" });
    try params.put("arguments", .{ .object = std.json.ObjectMap.init(alloc) });

    const response = try router.dispatch("tools/call", .{ .object = params });

    const err = response.@"error" orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(i64, -32601), err.code);
}

// ============================================================================
// Configuration Tests
// ============================================================================

test "RuntimeConfig parses valid JSON" {
    var runtime_config = RuntimeConfig{
        .allocator = testing.allocator,
        .std_path = null,
        .project_root = ".",
        .source_dirs = &.{},
        .verbose = false,
        .owned_strings = .{},
    };
    defer runtime_config.deinit();

    const json =
        \\{
        \\  "std_path": "/usr/local/klar/std",
        \\  "verbose": true
        \\}
    ;

    try runtime_config.parseConfigJson(json);

    try testing.expectEqualStrings("/usr/local/klar/std", runtime_config.std_path.?);
    try testing.expect(runtime_config.verbose);
}

test "RuntimeConfig defaults when no config file" {
    var runtime_config = try RuntimeConfig.init(testing.allocator);
    defer runtime_config.deinit();

    // Should use defaults
    try testing.expectEqualStrings(".", runtime_config.project_root);
    try testing.expect(!runtime_config.verbose);
}

// ============================================================================
// Standard Library Tests
// ============================================================================

const compiler = @import("compiler");
const analysis = @import("analysis");

test "parser handles Option type syntax" {
    const source =
        \\fn find(list: List<i32>, value: i32) -> Option<usize> {
        \\    None
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.root != compiler.ast.null_node);
}

test "parser handles Result type syntax" {
    const source =
        \\fn divide(a: i32, b: i32) -> Result<i32, String> {
        \\    if b == 0 {
        \\        Err("division by zero")
        \\    } else {
        \\        Ok(a / b)
        \\    }
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.root != compiler.ast.null_node);
}

test "parser handles generic struct with multiple type params" {
    const source =
        \\struct Pair<A, B> {
        \\    first: A,
        \\    second: B
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.root != compiler.ast.null_node);
}

test "parser handles generic enum" {
    const source =
        \\enum Option<T> {
        \\    Some(T),
        \\    None
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.root != compiler.ast.null_node);
}

test "parser handles trait definition" {
    const source =
        \\trait Display {
        \\    fn display(self) -> String
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.root != compiler.ast.null_node);
}

test "parser handles impl with trait" {
    const source =
        \\impl Display for Point {
        \\    fn display(self) -> String {
        \\        "Point"
        \\    }
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.root != compiler.ast.null_node);
}

test "parser handles method call on generic type" {
    const source =
        \\fn main() {
        \\    let maybe: Option<i32> = Some(42)
        \\    let value = maybe.unwrap_or(0)
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.root != compiler.ast.null_node);
}

test "parser handles match expression with enum variants" {
    const source =
        \\fn process(opt: Option<i32>) -> i32 {
        \\    match opt {
        \\        Some(n) => n,
        \\        None => 0
        \\    }
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.root != compiler.ast.null_node);
}

test "checker resolves primitive types" {
    const source =
        \\let x: i32 = 42
        \\let y: bool = true
        \\let z: str = "hello"
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    var checker_inst = try compiler.checker.Checker.init(testing.allocator, &tree);
    defer checker_inst.deinit();

    try checker_inst.check();

    // Should type check without errors
    try testing.expect(!checker_inst.hasErrors());
}

test "checker detects undefined type" {
    const source =
        \\let x: UndefinedType = 42
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    var checker_inst = try compiler.checker.Checker.init(testing.allocator, &tree);
    defer checker_inst.deinit();

    try checker_inst.check();

    // Should have errors for undefined type
    try testing.expect(checker_inst.hasErrors());
}

// ============================================================================
// Advanced Language Feature Tests (from std library)
// ============================================================================

test "parser handles pub use statement" {
    const source =
        \\pub use std.option.Option
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.errors.items.len == 0);
}

test "parser handles trait with associated type" {
    const source =
        \\trait Iterator {
        \\    type Item
        \\    fn next(self) -> Option[Self.Item]
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.errors.items.len == 0);
}

test "parser handles trait inheritance" {
    const source =
        \\trait Ord: Eq {
        \\    fn cmp(self, other: Self) -> Ordering
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.errors.items.len == 0);
}

test "parser handles where clause on impl" {
    const source =
        \\impl[T, U] T: Into[U]
        \\where U: From[T]
        \\{
        \\    fn into(self) -> U {
        \\        return U.from(self)
        \\    }
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.errors.items.len == 0);
}

test "parser handles question mark operator" {
    const source =
        \\fn get_value(opt: Option[i32]) -> Option[i32] {
        \\    let x = opt?
        \\    return Some(x + 1)
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.errors.items.len == 0);
}

test "parser handles while loop" {
    // Note: "while let" is not in reference Klar spec - using regular while loop
    const source =
        \\fn drain() {
        \\    var running: bool = true
        \\    while running {
        \\        process()
        \\    }
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.errors.items.len == 0);
}

test "parser handles wildcard pattern in for loop" {
    const source =
        \\fn skip_n(n: usize) {
        \\    for _ in 0..n {
        \\        do_something()
        \\    }
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.errors.items.len == 0);
}

test "parser handles dereference expression" {
    const source =
        \\fn deref_test(ptr: &i32) -> i32 {
        \\    *ptr
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.errors.items.len == 0);
}

test "parser handles trait with default method implementation" {
    const source =
        \\trait Eq {
        \\    fn eq(self, other: Self) -> bool
        \\
        \\    fn ne(self, other: Self) -> bool {
        \\        return not self.eq(other)
        \\    }
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.errors.items.len == 0);
}

test "parser handles generic trait" {
    const source =
        \\trait PartialEq[Rhs] {
        \\    fn eq(self, other: Rhs) -> bool
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.errors.items.len == 0);
}

test "parser handles Self.Item associated type access" {
    const source =
        \\fn get_next(self) -> Option[Self.Item] {
        \\    return None
        \\}
    ;

    var parser = compiler.parser.Parser.init(testing.allocator, source, 0);
    defer parser.deinit();

    var tree = try parser.parse();
    defer tree.deinit();

    // Should parse without errors
    try testing.expect(tree.errors.items.len == 0);
}
