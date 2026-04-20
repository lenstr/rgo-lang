# rgo

`rgo` is an experimental language with Rust-like syntax that targets Go.

The core idea is to let you write code in a more expressive, Rust-inspired syntax and still end up with **readable, idiomatic Go** that can be built with ordinary `go build`.

The biggest practical benefit is compilation speed: `rgo` lowers to Go, so the final build step keeps the **fast edit/compile/run loop of the Go toolchain** instead of introducing a heavy custom backend or runtime.

- **File extension:** `.rg`
- **CLI name:** `rgoc`
- **Compiler implementation:** OCaml 5.x
- **Target:** Go 1.26+

## Language capabilities

The compiler currently supports the full feature set described in the PRD (Phases 0–8):

- Functions, `let` bindings, mutability, control flow (`if`/`else`, `while`, `loop`, `for`)
- Structs with fields and `impl` blocks (methods + associated functions)
- Enums with tuple, struct, and unit variants
- Pattern matching (`match`) with exhaustiveness checking
- `Option<T>` and `Result<T, E>` as built-in sum types
- The `?` operator for early-return error/option propagation
- Generics with type parameters on structs, functions, and impl blocks
- Traits (`trait`, `impl Trait for Type`, default methods, generic bounds)
- Collections: `Vec<T>`, `HashMap<K, V>`, array literals
- Visibility (`pub`)
- Go code generation with `gofmt`-clean, deterministic output

## Quick start

All project commands run through the Nix flake:

```bash
# Enter the dev shell (optional — you can also prefix each command)
nix develop

# Build the compiler
nix develop -c dune build

# Run the test suite
nix develop -c dune runtest

# Check formatting
nix develop -c dune build @fmt

# Print the compiler version
nix develop -c dune exec rgoc -- --version
```

## Compiling an `.rg` file

```bash
nix develop -c dune exec rgoc -- examples/hello.rg -o hello.go
go run hello.go
```

The compiler reads a single `.rg` source file, runs the full pipeline (lex → parse → resolve → typecheck → exhaustiveness → codegen), writes Go source, and applies `gofmt`.

## Example

`examples/hello.rg`:

```rust
fn main() {
    println("Hello, world!");
}
```

Generated Go:

```go
package main

import "fmt"

func main() {
	fmt.Println("Hello, world!")
}
```

### Enum + match

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

### Result + `?` operator

```rust
fn divide(a: i64, b: i64) -> Result<i64, str> {
    if b == 0 {
        return Err("division by zero");
    }
    Ok(a / b)
}

fn half_divide(a: i64, b: i64) -> Result<i64, str> {
    let n = divide(a, b)?;
    Ok(n / 2)
}
```

### Generics + impl

```rust
struct Box<T> {
    value: T,
}

impl<T> Box<T> {
    pub fn new(v: T) -> Self {
        Box { value: v }
    }

    pub fn get(&self) -> T {
        self.value
    }
}
```

### Traits

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
```

More examples live in the `examples/` directory.

## Project structure

```
bin/          CLI entry point (rgoc)
lib/          Compiler library modules
  lexer.ml        Unicode-aware lexer (sedlex)
  parser.mly      Menhir grammar
  ast.ml          AST types
  resolver.ml     Name resolution + symbol tables
  typecheck.ml    Type checking + inference
  exhaust.ml      Match exhaustiveness checker
  codegen.ml      Go code emitter
  driver.ml       Pipeline orchestration
test/         Unit, snapshot, and e2e tests
examples/     Sample .rg programs
docs/         PRD and design decisions
```

## Testing

The test suite covers each compiler phase:

- **Lexer**: tokens, positions, nested comments, Unicode identifiers
- **Parser**: AST snapshots for all language constructs
- **Resolver**: symbol resolution, duplicate/undefined diagnostics
- **Typechecker**: type inference, error messages, generics, traits
- **Exhaustiveness**: missing-pattern diagnostics for match expressions
- **Codegen**: snapshot tests of generated Go, Go build/run validation
- **E2E**: full `.rg` -> `.go` -> `go build` -> `go run` round-trips

Run everything:

```bash
nix develop -c dune runtest
```

## Validation workflow

The compiler pipeline validates `.rg` programs through these phases in order:

1. **Lexing** — tokenize with position tracking
2. **Parsing** — produce a typed AST
3. **Name resolution** — resolve identifiers, detect undefined/duplicate symbols
4. **Type checking** — infer and check types, validate trait bounds
5. **Exhaustiveness** — reject non-exhaustive `match` expressions
6. **Code generation** — emit idiomatic Go, apply `gofmt`

If any phase fails, the compiler reports the error with file, line, and column information and exits non-zero.

## Documentation

The detailed product requirements and technical plan:

- `docs/PRD_rust_syntax_go_target_5.md`
