# Architecture

High-level architecture for the interop-first mission.

**What belongs here:** major compiler components, data flow, interop boundaries, planner/codegen handoff, and mission-level invariants.
**What does NOT belong here:** patch-level implementation notes, exhaustive API inventories, or exact validation commands/ports.

---

## Compiler pipeline

The repository remains a single-file compiler from one `.rg` input to one Go output, but this mission adds an explicit interop-aware semantic path rather than treating Go calls as codegen-only string substitutions.

1. `Lexer` tokenizes source, including the `use a::b` import surface and `::`-qualified references.
2. `Parser` builds AST forms for imports, package-qualified paths, callable expressions, and the minimum anonymous-function shape needed by the HTTP slice.
3. `Resolver` distinguishes symbol origin:
   - local rgo declarations
   - approved stdlib package namespaces such as `net::http`
   - future external Go packages behind a deferred boundary
4. `Typecheck` validates local types plus the approved imported Go surface, including package-qualified types, callable signatures, receiver/member access, and handler registration signatures.
5. `Interop planning` classifies each foreign reference before codegen:
   - import path to emit in Go
   - rgo-facing name to Go-facing name lowering
   - call-boundary ownership behavior
   - callback lowering mode
6. `Ownership / cleanup planning` preserves explicit move/copy/clone/drop decisions for the same program, including interop-sensitive by-value boundaries.
7. `Exhaust` remains downstream of typing and should stay orthogonal to package interop.
8. `Codegen` emits real Go imports and operations from the precomputed plans instead of rediscovering package, naming, ownership, or callback semantics ad hoc.
9. `Driver` orchestrates the phases, writes the output file, and keeps generated Go formatter-stable.

## Interop semantic layers

The mission should keep interop as a stack of semantic layers with clear responsibility boundaries.

### Import and namespace layer

- `use net::http` introduces a package namespace named `http`.
- Imported package members remain package-qualified in user code; bare imported-call resolution is out of scope.
- Imported package aliases are reserved at file scope; colliding top-level user declarations should fail explicitly instead of shadowing silently.
- Unsupported package paths should fail explicitly as deferred external-package work, not fall through as local-name errors.

### Import and naming bridge

- The rgo-facing surface uses Rust-style `snake_case` for imported functions and receiver methods.
- Imported Go type names stay PascalCase in rgo source.
- Source-level imported type names intentionally hide Go pointer/reference spelling when that improves the rgo surface; interop metadata must record the real lowered Go shape, for example `http::Request` -> `*http.Request` and `http::ResponseWriter` -> `http.ResponseWriter`.
- The bridge is directional: resolution/typecheck work in the rgo-facing namespace, while codegen lowers to real Go names such as `http.ListenAndServe`, `http.NewServeMux`, `mux.HandleFunc`, `req.FormValue`, and `w.WriteHeader`.

### Typed interop surface

- Package-qualified imported types must be valid in type position.
- Imported functions, constructors, and receiver members must be valid in expression position in the same program.
- Imported symbols should carry enough metadata for later phases to know whether they are:
  - type-only
  - value/callable
  - receiver member surface
  - callback-registration entry points

## Ownership and callback boundaries

This mission keeps ownership and callback work intentionally narrow but explicit.

### Ownership boundary

- By-value call semantics must be extensible by callee kind: local rgo callable, approved stdlib callable, and future foreign callable.
- Whole-binding ownership rules still apply first; no partial moves from fields or enum payloads.
- `Copy`, `Clone`, and `Drop` remain explicit semantic decisions, not codegen guesses.
- Interop calls must consume planner output that says whether a boundary is copy-preserving, move-producing, or cleanup-neutral.

### Callback slice

- Support passing named rgo functions directly where the approved `net/http` handler signature requires it.
- Support zero-capture anonymous handlers only.
- Reject capturing anonymous handlers in this mission.
- Callback registration must not consume reusable named function bindings or zero-capture handler values.

### Planner-to-codegen handoff

Interop and ownership planning should hand codegen an explicit artifact, such as annotated typed nodes or a per-function lowering plan. The handoff must carry at least:

- resolved package/import identity
- rgo-facing name to Go-facing name mapping
- imported type/value/member category
- callback lowering strategy for each registered handler
- call-boundary ownership classification
- cleanup suppression/scheduling decisions already computed upstream

Codegen should be a consumer of these decisions, not the place where they are recreated.

## HTTP slice

The first concrete interop slice is `net/http`, chosen as the validation surface for “real Go interop”.

### Supported mission shape

- import: `use net::http`
- server startup via `http::listen_and_serve`
- mux creation via `http::new_serve_mux`
- handler registration via the approved `ServeMux` receiver surface
- handler parameters using `http::ResponseWriter` and `http::Request`
- request/member operations needed for CRUD validation, including request method access and form-field reads
- response writing/status operations needed for CRUD validation

### Validation-facing behavior

The architecture must support a minimal CRUD-style server that proves:

- imports lower to real `net/http`
- package-qualified names compose in both type and value positions
- named handlers and zero-capture anonymous handlers both work
- repeated requests reuse registered handlers safely
- long-lived CRUD state is represented through explicit module-level or otherwise globally reachable state, not captured closures
- GET/POST success flows and error-path flows remain observable through real HTTP traffic

## External-package foundation

This mission is stdlib-first, but the semantic model should already leave room for arbitrary Go packages later.

- Symbol origin must be represented as more than “local vs not local”.
- Package-qualified lookup should not assume `net/http` is the only possible foreign namespace forever.
- Resolver/typecheck should have a clean deferred branch for unsupported external packages.
- Version resolution, package loading, and broad third-party validation are intentionally deferred.

## Key invariants

- Imported package namespaces stay explicit and package-qualified.
- The naming bridge always presents callables/members in `snake_case` and imported Go types in PascalCase.
- Ownership decisions are computed before codegen and preserved across interop boundaries.
- Callback support is limited to named functions and zero-capture anonymous handlers.
- The HTTP slice is the minimum needed to validate a real CRUD-style localhost server.
- External-package support is foundation-only in this mission.
- Generated Go must remain deterministic, formatter-stable, buildable, and vet-clean.
