//! klar://std/docs resource - Standard library documentation.
//!
//! This resource provides documentation for the Klar standard library,
//! including built-in types, functions, and modules.

const std = @import("std");
const Allocator = std.mem.Allocator;

const log = std.log.scoped(.std_docs);

/// Resource URI for standard library documentation.
pub const uri = "klar://std/docs";

/// Resource name for display.
pub const name = "Klar Standard Library Documentation";

/// Resource description.
pub const description = "Documentation for the Klar standard library, including built-in types, functions, and modules.";

/// MIME type for the resource.
pub const mime_type = "text/markdown";

/// Execute the resource read operation.
/// Returns the standard library documentation as a JSON value.
pub fn execute(allocator: Allocator, requested_uri: []const u8) !std.json.Value {
    _ = requested_uri; // For now, we only have one docs resource

    const docs_content = getStdLibDocs();

    // Build MCP resource response
    // MCP resources/read returns { contents: [{ uri, mimeType, text }] }
    var contents_arr = std.json.Array.init(allocator);

    var content_obj = std.json.ObjectMap.init(allocator);
    try content_obj.put("uri", .{ .string = uri });
    try content_obj.put("mimeType", .{ .string = mime_type });
    try content_obj.put("text", .{ .string = docs_content });
    try contents_arr.append(.{ .object = content_obj });

    var result = std.json.ObjectMap.init(allocator);
    try result.put("contents", .{ .array = contents_arr });

    return .{ .object = result };
}

/// Get the standard library documentation content.
fn getStdLibDocs() []const u8 {
    return
        \\# Klar Standard Library Documentation
        \\
        \\## Overview
        \\
        \\The Klar standard library provides essential types, functions, and modules
        \\for building Klar programs. This documentation covers the built-in
        \\primitives and standard modules.
        \\
        \\## Built-in Types
        \\
        \\### Integer Types
        \\
        \\| Type | Description | Range |
        \\|------|-------------|-------|
        \\| `i8` | 8-bit signed integer | -128 to 127 |
        \\| `i16` | 16-bit signed integer | -32,768 to 32,767 |
        \\| `i32` | 32-bit signed integer | -2^31 to 2^31-1 |
        \\| `i64` | 64-bit signed integer | -2^63 to 2^63-1 |
        \\| `u8` | 8-bit unsigned integer | 0 to 255 |
        \\| `u16` | 16-bit unsigned integer | 0 to 65,535 |
        \\| `u32` | 32-bit unsigned integer | 0 to 2^32-1 |
        \\| `u64` | 64-bit unsigned integer | 0 to 2^64-1 |
        \\| `usize` | Pointer-sized unsigned | Platform dependent |
        \\| `isize` | Pointer-sized signed | Platform dependent |
        \\
        \\### Floating Point Types
        \\
        \\| Type | Description |
        \\|------|-------------|
        \\| `f32` | 32-bit floating point (IEEE 754) |
        \\| `f64` | 64-bit floating point (IEEE 754) |
        \\
        \\### Other Primitive Types
        \\
        \\| Type | Description |
        \\|------|-------------|
        \\| `bool` | Boolean type (`true` or `false`) |
        \\| `void` | Unit type (no value) |
        \\| `never` | Bottom type (never returns) |
        \\
        \\## String Types
        \\
        \\```klar
        \\// String literal (immutable)
        \\let greeting: String = "Hello, World!"
        \\
        \\// Character literal
        \\let ch: char = 'A'
        \\```
        \\
        \\### String Operations
        \\
        \\- `len()` - Returns the length of the string
        \\- `is_empty()` - Returns true if the string is empty
        \\- `concat(other)` - Concatenates two strings
        \\- `contains(substr)` - Checks if string contains substring
        \\- `starts_with(prefix)` - Checks if string starts with prefix
        \\- `ends_with(suffix)` - Checks if string ends with suffix
        \\
        \\## Collections
        \\
        \\### List<T>
        \\
        \\A dynamic array that can grow and shrink.
        \\
        \\```klar
        \\let numbers: List<i32> = [1, 2, 3, 4, 5]
        \\numbers.push(6)
        \\let first = numbers[0]  // 1
        \\```
        \\
        \\**Methods:**
        \\- `push(item)` - Adds an item to the end
        \\- `pop()` - Removes and returns the last item
        \\- `len()` - Returns the number of items
        \\- `is_empty()` - Returns true if empty
        \\- `clear()` - Removes all items
        \\- `get(index)` - Returns item at index (Option<T>)
        \\
        \\### Map<K, V>
        \\
        \\A hash map for key-value pairs.
        \\
        \\```klar
        \\let ages: Map<String, i32> = {}
        \\ages.insert("Alice", 30)
        \\ages.insert("Bob", 25)
        \\let alice_age = ages.get("Alice")  // Option<i32>
        \\```
        \\
        \\**Methods:**
        \\- `insert(key, value)` - Inserts or updates a key-value pair
        \\- `get(key)` - Returns value for key (Option<V>)
        \\- `remove(key)` - Removes a key-value pair
        \\- `contains(key)` - Checks if key exists
        \\- `len()` - Returns the number of pairs
        \\- `keys()` - Returns iterator over keys
        \\- `values()` - Returns iterator over values
        \\
        \\## Option<T>
        \\
        \\Represents an optional value that may or may not be present.
        \\
        \\```klar
        \\let maybe_number: Option<i32> = Some(42)
        \\let nothing: Option<i32> = None
        \\
        \\// Pattern matching
        \\match maybe_number {
        \\    Some(n) => print(n),
        \\    None => print("No value")
        \\}
        \\
        \\// Unwrap with default
        \\let value = maybe_number.unwrap_or(0)
        \\```
        \\
        \\**Methods:**
        \\- `is_some()` - Returns true if contains a value
        \\- `is_none()` - Returns true if empty
        \\- `unwrap()` - Returns the value (panics if None)
        \\- `unwrap_or(default)` - Returns value or default
        \\- `map(fn)` - Transforms the inner value
        \\
        \\## Result<T, E>
        \\
        \\Represents either success or failure.
        \\
        \\```klar
        \\fn divide(a: i32, b: i32) -> Result<i32, String> {
        \\    if b == 0 {
        \\        Err("Division by zero")
        \\    } else {
        \\        Ok(a / b)
        \\    }
        \\}
        \\
        \\// Pattern matching
        \\match divide(10, 2) {
        \\    Ok(result) => print(result),
        \\    Err(msg) => print("Error: " + msg)
        \\}
        \\
        \\// Propagate errors with ?
        \\fn compute() -> Result<i32, String> {
        \\    let x = divide(10, 2)?
        \\    Ok(x * 2)
        \\}
        \\```
        \\
        \\**Methods:**
        \\- `is_ok()` - Returns true if success
        \\- `is_err()` - Returns true if error
        \\- `unwrap()` - Returns success value (panics on error)
        \\- `unwrap_err()` - Returns error value (panics on success)
        \\- `map(fn)` - Transforms the success value
        \\- `map_err(fn)` - Transforms the error value
        \\
        \\## Control Flow
        \\
        \\### If Expression
        \\
        \\```klar
        \\let result = if condition {
        \\    value_if_true
        \\} else {
        \\    value_if_false
        \\}
        \\```
        \\
        \\### Match Expression
        \\
        \\```klar
        \\match value {
        \\    Pattern1 => result1,
        \\    Pattern2 => result2,
        \\    _ => default_result
        \\}
        \\```
        \\
        \\### Loops
        \\
        \\```klar
        \\// While loop
        \\while condition {
        \\    // body
        \\}
        \\
        \\// For loop over range
        \\for i in 0..10 {
        \\    print(i)
        \\}
        \\
        \\// For loop over collection
        \\for item in list {
        \\    process(item)
        \\}
        \\```
        \\
        \\## Functions
        \\
        \\### Function Declaration
        \\
        \\```klar
        \\fn greet(name: String) -> String {
        \\    "Hello, " + name + "!"
        \\}
        \\
        \\// Function with no return value
        \\fn log(message: String) {
        \\    print(message)
        \\}
        \\
        \\// Generic function
        \\fn identity<T>(value: T) -> T {
        \\    value
        \\}
        \\```
        \\
        \\## Structs
        \\
        \\```klar
        \\struct Point {
        \\    x: f64,
        \\    y: f64
        \\}
        \\
        \\impl Point {
        \\    fn new(x: f64, y: f64) -> Point {
        \\        Point { x, y }
        \\    }
        \\
        \\    fn distance(self, other: Point) -> f64 {
        \\        let dx = self.x - other.x
        \\        let dy = self.y - other.y
        \\        (dx * dx + dy * dy).sqrt()
        \\    }
        \\}
        \\```
        \\
        \\## Enums
        \\
        \\```klar
        \\enum Color {
        \\    Red,
        \\    Green,
        \\    Blue,
        \\    Rgb(u8, u8, u8)
        \\}
        \\
        \\let color = Color::Rgb(255, 128, 0)
        \\
        \\match color {
        \\    Color::Red => print("Red"),
        \\    Color::Rgb(r, g, b) => print("RGB: " + r + ", " + g + ", " + b),
        \\    _ => print("Other")
        \\}
        \\```
        \\
        \\## Traits
        \\
        \\```klar
        \\trait Display {
        \\    fn display(self) -> String
        \\}
        \\
        \\impl Display for Point {
        \\    fn display(self) -> String {
        \\        "(" + self.x.to_string() + ", " + self.y.to_string() + ")"
        \\    }
        \\}
        \\```
        \\
        \\## Modules
        \\
        \\```klar
        \\// Importing a module
        \\import std.io
        \\import std.collections.List
        \\
        \\// Using imported items
        \\let file = io.open("data.txt")
        \\```
        \\
        \\## Standard Modules
        \\
        \\| Module | Description |
        \\|--------|-------------|
        \\| `std.io` | Input/output operations |
        \\| `std.fs` | File system operations |
        \\| `std.collections` | Collection types (List, Map, Set) |
        \\| `std.math` | Mathematical functions |
        \\| `std.string` | String utilities |
        \\| `std.time` | Time and date operations |
        \\| `std.json` | JSON parsing and serialization |
        \\
    ;
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

    // Check that text content exists and is non-empty
    const text = content.get("text").?.string;
    try std.testing.expect(text.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, text, "Klar Standard Library") != null);
}

test "docs contain expected sections" {
    const docs = getStdLibDocs();

    // Check for major sections
    try std.testing.expect(std.mem.indexOf(u8, docs, "## Built-in Types") != null);
    try std.testing.expect(std.mem.indexOf(u8, docs, "## Collections") != null);
    try std.testing.expect(std.mem.indexOf(u8, docs, "## Option<T>") != null);
    try std.testing.expect(std.mem.indexOf(u8, docs, "## Result<T, E>") != null);
    try std.testing.expect(std.mem.indexOf(u8, docs, "## Functions") != null);
    try std.testing.expect(std.mem.indexOf(u8, docs, "## Structs") != null);
    try std.testing.expect(std.mem.indexOf(u8, docs, "## Enums") != null);
    try std.testing.expect(std.mem.indexOf(u8, docs, "## Traits") != null);
}
