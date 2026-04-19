# Architecture

How the system works at a high level.

**What belongs here:** compiler phases, major modules, data flow, invariants.
**What does NOT belong here:** exact command invocations or service ports.

---

## Compiler pipeline

The repository implements a single-file compiler pipeline for `rgo` source programs, with one `.rg` input file lowering to one Go package whose default package naming derives from the source filename.

1. `Lexer` tokenizes `.rg` source using sedlex.
2. `Parser` turns tokens into an AST using Menhir.
3. `Resolver` assigns names to declarations and uses, including type names, enum variants, impl scopes, and trait method namespaces.
4. `Typecheck` validates the AST, performs local inference, enforces mutability rules, checks `Option`/`Result`/`?`, resolves method calls, validates trait impls and bounds, and substitutes `Self` where allowed.
5. `Exhaust` validates exhaustiveness of typed pattern matches, primarily for enums/sum types, after type information is available.
6. `Codegen` lowers typed rgo programs into formatted Go source, emits any required helper prelude definitions/imports, and preserves PRD mapping rules.
7. `Driver` and the CLI glue phases together, run output formatting, and write output files.

## Planned repository layout

- `bin/` holds the CLI entrypoint (`rgoc`).
- `lib/` holds compiler modules and their `.mli` interfaces.
- `test/` holds unit, snapshot, negative, and end-to-end fixtures.
- `examples/` holds sample `.rg` programs that should compile through the pipeline.

## Core invariants

- All user-facing compiler entrypoints run via the flake-provided toolchain.
- The compiler targets Go 1.26+; trait lowering that uses `Self` depends on that version floor.
- The typed AST is the source of truth for Phase 7+ code generation.
- Nullability classification is a cross-phase invariant: the same rgo type must map consistently to pointer-backed vs struct-backed `Option<T>` representations across params, returns, fields, and nested types.
- Generated Go must be deterministic for identical inputs.
- Generated Go must be formatter-stable and compatible with `go build`/`go vet`.
- No custom runtime is introduced; the compiler targets ordinary Go constructs and standard tooling.

## Trait architecture

Traits affect parsing, resolution, typechecking, and code generation:
- trait declarations introduce method contracts and optional default bodies;
- `impl Trait for Type` blocks must be checked for completeness and signature compatibility;
- generic bounds must be enforced before code generation;
- traits using `Self` lower to Go 1.26-compatible self-referential generic interfaces.

## Code generation shape

- Structs lower to Go structs with visibility mapped from `pub`.
- Enums lower to sealed-interface patterns with concrete variant types.
- `Option<T>` lowers either to pointers or a generated generic `Option[T]` struct depending on payload nullability.
- `Result<T, str>` lowers to `(T, error)`.
- `match` lowers to Go switches or type switches, using IIFEs when an expression result is needed.
- Inherent impls and trait impls lower to Go methods/functions while preserving the PRD’s call semantics.
- Codegen emits a minimal per-file helper/prelude surface only when needed (for example `Option[T]`, `rgo_some`, `rgo_none`, `rgo_repeat`, and supporting imports).
