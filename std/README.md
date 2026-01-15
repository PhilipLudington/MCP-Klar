# Klar Standard Library

This directory contains the Klar standard library source files. The standard library provides essential types, traits, and functions for Klar programs.

## Module Structure

```
std/
├── prelude.kl        # Commonly used types, auto-imported
├── option.kl         # Option<T> type for nullable values
├── result.kl         # Result<T, E> type for error handling
├── string.kl         # String type and utilities
├── traits.kl         # Core traits (Display, Clone, Eq, etc.)
├── collections/
│   ├── list.kl       # List<T> dynamic array
│   ├── map.kl        # Map<K, V> hash map
│   └── set.kl        # Set<T> hash set
└── io/
    ├── mod.kl        # I/O module re-exports
    ├── reader.kl     # Reader trait and BufReader
    ├── writer.kl     # Writer trait and BufWriter
    ├── stdio.kl      # stdin, stdout, stderr
    └── file.kl       # File I/O operations
```

## Core Types

### Option<T>

Represents an optional value.

```klar
let maybe: Option<i32> = Some(42)
let nothing: Option<i32> = None

// Pattern matching
match maybe {
    Some(n) => println(n.to_string()),
    None => println("no value")
}

// Methods
maybe.is_some()          // true
maybe.unwrap()           // 42
maybe.unwrap_or(0)       // 42
nothing.unwrap_or(0)     // 0
maybe.map(|n| n * 2)     // Some(84)
```

### Result<T, E>

Represents success or failure.

```klar
fn divide(a: i32, b: i32) -> Result<i32, String> {
    if b == 0 {
        Err("division by zero")
    } else {
        Ok(a / b)
    }
}

// Error propagation with ?
fn compute() -> Result<i32, String> {
    let x = divide(10, 2)?
    let y = divide(x, 0)?  // Returns Err early
    Ok(y)
}

// Pattern matching
match divide(10, 2) {
    Ok(n) => println("Result: " + n.to_string()),
    Err(e) => println("Error: " + e)
}
```

### String

A growable UTF-8 string.

```klar
let mut s = String::new()
s.push_str("Hello")
s.push(' ')
s.push_str("World")

s.len()                  // 11
s.is_empty()             // false
s.contains("World")      // true
s.starts_with("Hello")   // true
s.to_uppercase()         // "HELLO WORLD"
s.split(" ")             // ["Hello", "World"]
```

### List<T>

A dynamic array.

```klar
let mut numbers: List<i32> = List::new()
numbers.push(1)
numbers.push(2)
numbers.push(3)

numbers.len()            // 3
numbers[0]               // 1
numbers.pop()            // Some(3)
numbers.contains(&2)     // true

// Iteration
for n in numbers.iter() {
    println(n.to_string())
}

// Functional operations
numbers.map(|n| n * 2)
numbers.filter(|n| n > 1)
numbers.fold(0, |acc, n| acc + n)
```

### Map<K, V>

A hash map for key-value pairs.

```klar
let mut ages: Map<String, i32> = Map::new()
ages.insert("Alice", 30)
ages.insert("Bob", 25)

ages.get("Alice")        // Some(&30)
ages.contains_key("Bob") // true
ages.remove("Bob")       // Some(25)

for (name, age) in ages.iter() {
    println(name + ": " + age.to_string())
}
```

### Set<T>

A hash set for unique values.

```klar
let mut numbers: Set<i32> = Set::new()
numbers.insert(1)
numbers.insert(2)
numbers.insert(1)        // Returns false, already present

numbers.len()            // 2
numbers.contains(&1)     // true

// Set operations
let a: Set<i32> = [1, 2, 3].into()
let b: Set<i32> = [2, 3, 4].into()
a.union(&b)              // {1, 2, 3, 4}
a.intersection(&b)       // {2, 3}
a.difference(&b)         // {1}
```

## Core Traits

### Display & Debug

```klar
trait Display {
    fn display(&self) -> String
}

trait Debug {
    fn debug(&self) -> String
}
```

### Clone & Eq

```klar
trait Clone {
    fn clone(&self) -> Self
}

trait Eq {
    fn eq(&self, other: &Self) -> bool
}
```

### Ord

```klar
enum Ordering { Less, Equal, Greater }

trait Ord: Eq {
    fn cmp(&self, other: &Self) -> Ordering
}
```

### Hash

```klar
trait Hash {
    fn hash(&self, hasher: &mut Hasher)
}
```

### Iterator

```klar
trait Iterator {
    type Item
    fn next(&mut self) -> Option<Self::Item>
}
```

## I/O Operations

### Reading from stdin

```klar
use std.io.{stdin, input}

// Read a line
let line = input()?

// With prompt
let name = input_with_prompt("Enter name: ")?

// Read bytes
let mut buf: [u8; 1024] = [0; 1024]
let n = stdin().read(&mut buf)?
```

### Writing to stdout/stderr

```klar
use std.io.{stdout, stderr, println, eprintln}

// Simple printing
println("Hello, World!")
eprintln("Error message")

// Direct write
stdout().write_str("Hello")?
stderr().write_line("Error")?
```

### File I/O

```klar
use std.io.file.{File, read_to_string, write_str}

// Read entire file
let contents = read_to_string("data.txt")?

// Write to file
write_str("output.txt", "Hello, World!")?

// Manual file handling
let mut file = File::open("data.txt")?
let mut contents = String::new()
file.read_to_string(&mut contents)?

// Create and write
let mut file = File::create("output.txt")?
file.write_all("Hello".as_bytes())?
file.flush()?

// Append mode
let mut file = File::append("log.txt")?
file.write_all("New entry\n".as_bytes())?
```

### Buffered I/O

```klar
use std.io.{BufReader, BufWriter}

// Buffered reading
let file = File::open("data.txt")?
let mut reader = BufReader::new(file)
for line in reader.lines() {
    println(line?)
}

// Buffered writing
let file = File::create("output.txt")?
let mut writer = BufWriter::new(file)
writer.write_all("Line 1\n".as_bytes())?
writer.write_all("Line 2\n".as_bytes())?
writer.flush()?
```

## Configuration

Set the `KLAR_STD_PATH` environment variable to point to this directory, or configure it in `klar.json`:

```json
{
  "std_path": "/path/to/std"
}
```

## Implementation Notes

- Types prefixed with `__builtin_` are provided by the runtime
- The standard library is designed to be type-checked by the MCP server
- All I/O operations return `IoResult<T>` which is `Result<T, Error>`
