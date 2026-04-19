# rgo

`rgo` is an experimental language with Rust-like syntax that targets Go.

The core idea is to let you write code in a more expressive, Rust-inspired syntax and still end up with **readable, idiomatic Go** that can be built with ordinary `go build`.

The biggest practical benefit is compilation speed: `rgo` lowers to Go, so the final build step keeps the **fast edit/compile/run loop of the Go toolchain** instead of introducing a heavy custom backend or runtime.

- **File extension:** `.rg`
- **CLI name:** `rgoc`
- **Compiler implementation:** OCaml 5.x
- **Target:** Go 1.26+

## Why this is interesting

`rgo` aims to combine:

- Rust-like syntax
- algebraic data types (`enum`)
- `Option<T>` / `Result<T, E>`
- `match`
- `impl`, `trait`, and generics
- the `?` operator

with practical Go advantages:

- **fast compilation via the standard Go toolchain**
- readable and idiomatic generated Go
- goroutines and Go's runtime model
- a mature standard library
- simple binary distribution

The goal is not to generate weird intermediate code that merely happens to compile.
The goal is to translate Rust-like source into Go that a Go developer can still read, debug, and maintain.

## Rust-like in, idiomatic Go out

### Example: function translation

`rgo`:

```rust
pub fn add(a: i64, b: i64) -> i64 {
    a + b
}
```

Generated Go:

```go
func Add(a int64, b int64) int64 {
    return a + b
}
```

### Example: `Result` + `?` to Go-style error handling

`rgo`:

```rust
fn double(s: str) -> Result<i64, str> {
    let n = parse_int(s)?;
    Ok(n * 2)
}
```

Generated Go:

```go
func Double(s string) (int64, error) {
    n, err := parseInt(s)
    if err != nil {
        return 0, err
    }
    return n * 2, nil
}
```

That translation style is a major part of the project vision: keep the input language modern and expressive, while keeping the output close to normal Go conventions.

## Project status

This repository is currently in the early prototype stage.

What already exists:

- a `dune`-based project skeleton
- a CLI with `--version`
- a Unicode-aware lexer built with `sedlex`
- tests for core tokens and position tracking

The README below shows the **intended language syntax**, meaning examples of what `rgo` is expected to look like as the compiler grows.

## Quick start

All project commands should be run through the Nix flake:

```bash
nix develop -c dune build
nix develop -c dune runtest
nix develop -c dune exec rgoc -- --version
```

Expected output:

```text
rgoc v0.0.1
```

## Syntax examples

> The examples below describe the target language design, even if some constructs are not implemented in the compiler yet.

### Hello world

```rust
fn main() {
    println("Hello, world!");
}
```

### Functions and `let`

```rust
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() {
    let x = add(20, 22);
    println(x);
}
```

### Mutable variables

```rust
fn main() {
    let mut total: i64 = 0;
    total = total + 10;
    println(total);
}
```

### Conditional expressions

```rust
fn abs(x: i64) -> i64 {
    if x < 0 {
        -x
    } else {
        x
    }
}
```

### Struct

```rust
struct Point {
    x: f64,
    y: f64,
}

fn distance_squared(a: Point, b: Point) -> f64 {
    let dx = a.x - b.x;
    let dy = a.y - b.y;
    dx * dx + dy * dy
}
```

### Enum and `match`

```rust
enum Shape {
    Circle(f64),
    Rectangle { width: f64, height: f64 },
    Empty,
}

fn area(shape: Shape) -> f64 {
    match shape {
        Shape::Circle(r) => 3.14 * r * r,
        Shape::Rectangle { width, height } => width * height,
        Shape::Empty => 0.0,
    }
}
```

### `Option<T>`

```rust
fn first(v: Vec<i64>) -> Option<i64> {
    if v.len() == 0 {
        return None;
    }

    Some(v[0])
}
```

### `Result<T, E>` and the `?` operator

```rust
fn parse_int(s: str) -> Result<i64, str> {
    // later this will bind to strconv.ParseInt
}

fn double(s: str) -> Result<i64, str> {
    let n = parse_int(s)?;
    Ok(n * 2)
}
```

### Methods with `impl`

```rust
struct Counter {
    value: i64,
}

impl Counter {
    pub fn new() -> Self {
        Counter { value: 0 }
    }

    pub fn get(&self) -> i64 {
        self.value
    }

    pub fn inc(&mut self) {
        self.value = self.value + 1;
    }
}
```

### Associated functions

```rust
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    pub fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }
}

fn main() {
    let p = Point::new(1.0, 2.0);
    println(p.x);
}
```

### Trait and generic bounds

```rust
trait Display {
    fn display(&self) -> String;
}

struct User {
    name: String,
}

impl Display for User {
    fn display(&self) -> String {
        self.name
    }
}

fn print_one<T: Display>(value: T) {
    println(value.display());
}
```

### Collections

```rust
fn main() {
    let xs: Vec<i64> = [1, 2, 3, 4];
    let empty: Vec<String> = [];
    let zeros: Vec<i64> = [0; 8];

    println(xs.len());
    println(empty.len());
    println(zeros.len());
}
```

### Loops

```rust
fn sum(xs: Vec<i64>) -> i64 {
    let mut total: i64 = 0;

    for x in xs {
        total = total + x;
    }

    total
}
```

### `while` and `loop`

```rust
fn countdown(mut n: i64) {
    while n > 0 {
        println(n);
        n = n - 1;
    }

    loop {
        break;
    }
}
```

## Example of the planned pipeline

In the target form, the compiler should support a workflow roughly like this:

```bash
rgoc examples/hello.rg -o hello.go
gofmt -w hello.go
go run hello.go
```

## What tests cover today

Right now, tests mainly cover the lexer:

- keywords (`fn`, `let`, `match`, `impl`, `trait`, etc.)
- built-in types (`i64`, `String`, `Option`, `Result`, `Vec`, `HashMap`)
- strings, numbers, and operators
- nested block comments
- Unicode identifiers
- token positions

Run them with:

```bash
nix develop -c dune runtest
```

## Next steps

The next major milestones are:

1. AST
2. parser with Menhir
3. name resolution
4. type checking
5. exhaustiveness checking for `match`
6. Go code generation

## Documentation

The detailed product and technical plan lives here:

- `docs/PRD_rust_syntax_go_target_5.md`

If you want a quick overview of the project direction, start with this README and then move on to the PRD.
