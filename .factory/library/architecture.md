# Architecture

How the system works at a high level.

**What belongs here:** compiler phases, major modules, data flow, invariants, and mission-relevant architectural boundaries.
**What does NOT belong here:** exact shell commands or service/port details.

---

## Compiler pipeline

The repository implements a single-file compiler pipeline for `rgo` source programs, with one `.rg` input lowering to one Go package.

Current and mission-relevant phases:

1. `Lexer` tokenizes `.rg` source using sedlex.
2. `Parser` turns tokens into an AST using Menhir.
3. `Resolver` assigns names to declarations and uses, including type names, enum variants, impl scopes, and trait method namespaces.
4. `Typecheck` validates the AST, performs local inference, enforces mutability rules, checks `Option`/`Result`/`?`, resolves method calls, validates trait impls and bounds, and substitutes `Self` where allowed.
5. `Ownership / Drop analysis` is the planned new mission layer. It should decide whole-binding move validity, Copy-vs-move behavior, Clone escape-hatch behavior, and cleanup scheduling for named bindings.
6. `Exhaust` validates exhaustiveness of typed pattern matches after type information is available.
7. `Codegen` lowers rgo programs into formatted Go source and must preserve the ownership/drop decisions made earlier.
8. `Driver` and the CLI glue phases together, run output formatting, and write output files.

## Planned ownership / cleanup architecture

This mission should treat `Drop`, `Copy`, and `Clone` as a coordinated semantic subsystem rather than three isolated parser or codegen tweaks.

### Semantic roles

- `Drop` introduces automatic cleanup for owned user-defined nominal values. Its contract recognition and impl-shape validation belong in semantic analysis: duplicate impls, missing required methods, invalid method shapes, and invalid impl targets should be rejected before codegen.
- `Copy` prevents ownership transfer at whole-binding move sites when the type is eligible. Its eligibility and bound validation also belong in semantic analysis.
- `Clone` is the explicit user-facing duplication escape hatch for non-`Copy` values. Its canonical `clone(&self) -> Self` contract should be validated in semantic analysis just like other required trait methods.

### MVP ownership boundary

The mission boundary is **whole named bindings only**:

- supported move sites: assignment/rebinding, by-value calls, and by-value returns
- supported cleanup paths: normal scope exit, mutable-binding overwrite, explicit `return`, by-value parameter cleanup at callee exit, and `?` exits including nested `?` expression forms covered by the contract
- moved bindings become unusable afterward
- no partial moves from fields or enum payloads
- no full borrow checker or lifetime analysis

### Cleanup scheduling model

Workers should preserve these invariants:

- cleanup only for still-live owned bindings
- cleanup in reverse declaration order on normal scope exit
- cleanup also on early exits covered by the mission (`return`, `?`, and equivalent control-flow exits the feature explicitly supports)
- overwriting a live mutable binding cleans up the old value exactly once
- moved bindings must not be cleaned up a second time

### Recommended implementation split

The cleanest architecture is to keep contract validation and type eligibility in semantic analysis, then have one explicit ownership/cleanup planning layer produce information that codegen can consume.

That means:

- parser/AST should change only if the language surface itself changes
- resolver/typecheck should recognize and validate the `Drop` / `Copy` / `Clone` contracts and determine type-level eligibility
- an ownership/drop planning layer should reason about binding lifetimes, moves, and cleanup points
- codegen should lower already-decided cleanup/copy/clone behavior rather than re-deriving semantics ad hoc from raw AST shape

### Planner-to-codegen handoff

Workers should not leave this implicit. The ownership/cleanup layer needs a concrete handoff artifact to codegen, such as:

- annotated typed AST nodes, or
- a separate per-function cleanup/move plan keyed by statements/expressions/bindings

Whatever representation is chosen, it must carry at least:

- which bindings are live vs moved at each move site
- where cleanup must run
- which cleanups are suppressed because ownership transferred elsewhere
- which paths are Copy-preserving rather than move-producing

The exact data structure is up to the implementation, but codegen should consume explicit precomputed decisions rather than re-infering ownership ad hoc.

## Core invariants for this mission

- `Drop` is only for supported user-defined nominal types in this mission (`struct`, `enum`, including generic nominal types).
- `Drop` types cannot also be `Copy`.
- `Clone` remains explicit and user-visible through `clone()`.
- Generic instantiations must stay concrete across move/copy/clone/drop paths; workers must not collapse instantiated types to bare nominal names.
- Downstream generated Go must remain deterministic, formatter-stable, buildable, and vet-clean.
- Existing trait/impl/generic behavior outside the new ownership semantics must not regress.

## Repository layout

- `bin/` holds the CLI entrypoint (`rgoc`).
- `lib/` holds compiler modules and their `.mli` interfaces.
- `test/` holds unit, snapshot, negative, and end-to-end fixtures.
- `examples/` holds sample `.rg` programs that should compile through the pipeline.

## Validation shape

The user-facing surface remains CLI-only:

- compile `.rg` fixtures/examples with `rgoc`
- validate generated Go with `gofmt -d`, `go build`, `go vet`, and `go run`
- use exact stdout ordering/counts as the oracle for Drop timing, exactly-once behavior, Copy reuse, Clone duplication, and cross-area early-exit flows
