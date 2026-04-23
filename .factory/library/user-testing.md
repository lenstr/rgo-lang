# User Testing

Testing surface, required tools, and concurrency assumptions.

**What belongs here:** how validators should exercise the system, what artifacts to inspect, and resource notes.
**What does NOT belong here:** implementation details of compiler modules.

---

## Validation Surface

This mission exposes three validation surfaces:

1. `rgoc` CLI invoked through `nix develop -c ...`
2. Generated Go artifacts produced from `.rg` fixtures and examples
3. A local HTTP server started from the generated CRUD validation fixture

Representative validation flows:
- compile success/failure through the real CLI
- verify failed compilations do not leave misleading output files behind
- compile `.rg` input to `.go` via `rgoc input.rg -o output.go`
- run `gofmt -d`, `go build`, `go vet`, and `go run` on generated output when appropriate
- compile `.factory/runtime/interop-crud.rg` to `./.factory/runtime/http-validation-server` for live HTTP validation
- use `.factory/runtime/interop-crud.rg` as the tracked interop-heavy fixture for both live HTTP validation and repeated `rgoc` output comparisons
- start the fixture on `127.0.0.1:3111`
- validate `GET /items`, `POST /items` with form field `name`, malformed `POST /items`, and unsupported-method traffic
- use exact stdout ordering/counts to validate move/copy/clone/drop behavior
- include both direct `?` flows and nested `?` flows when validating cleanup on early exit

## Validation Concurrency

- Surface: `rgoc` CLI / generated Go / local HTTP fixture
- Max concurrent validators: 1
- Rationale: although the host has ample CPU and memory headroom, validation shares one repository working tree, one generated runtime binary path, and one reserved localhost port (`3111`), so serial execution is the safest non-flaky configuration.

## Flow Validator Guidance

- Stay inside the repository root and use `nix develop -c ...` for all commands.
- Use `.factory/services.yaml` as the source of truth for the reserved HTTP validation port and runtime binary path.
- For live HTTP probes, compile `.factory/runtime/interop-crud.rg` to the shared `http-validation-server` binary path so automation and manual validation exercise the same tracked CRUD fixture.
- Bind the generated HTTP fixture to `127.0.0.1:3111` only.
- Prefer `curl` for HTTP assertions and capture full response status/body for create/read/error paths.
- Treat `_build/`, `.factory/runtime/`, temporary generated Go files, and the localhost validation port as shared mutable state; run validations serially unless explicit isolated copies are provisioned.
- For ownership-sensitive flows, prefer fixtures whose stdout makes ownership transfer and cleanup timing obvious (distinct markers per binding or path).

## Flow Validator Guidance: rgoc CLI + generated Go artifacts

- Keep validation serial: this surface shares one repo worktree, one `_build/`, and one generated-artifact area.
- Write temporary `.rg`, `.go`, and log artifacts under the mission evidence directory, not into tracked source paths.
- When evidence outputs live outside the repository root, still invoke `nix develop -c ...` from the repo root (or another directory inside the flake) and pass absolute paths to the evidence artifacts; running `nix develop` directly from the evidence directory will fail flake discovery.
- For positive import-surface assertions, prove behavior through the real CLI plus downstream Go checks (`gofmt -d`, `go build`, and `go vet` when the contract requires them).
- For negative import-surface assertions, verify both the user-facing diagnostic text and the absence of a misleading output file at the requested `-o` path.
- Use representative fixtures that exercise both package-qualified callable names (`http::listen_and_serve`, `http::new_serve_mux`) and PascalCase type names (`http::Request`, `http::ResponseWriter`) in the same flow where required.

## Flow Validator Guidance: rgoc CLI + generated Go ownership flows

- Keep validation serial: ownership validation shares one repo worktree, one `_build/`, and one generated-artifact area.
- Use the real CLI (`nix develop -c dune exec rgoc -- ...`) plus downstream Go tooling (`gofmt -d`, `go build`, `go vet`, `go run`) for positive fixtures, and verify negative fixtures produce no output file at the requested `-o` path.
- Write per-assertion fixtures, generated Go, stdout captures, and stderr captures under the mission evidence directory for `ownership-boundaries`; avoid tracked source paths.
- Prefer the already-curated ownership fixtures in `test/test_codegen_ownership.ml`, `test/test_typecheck.ml`, and `examples/generic_ownership.rg`, `examples/generic_enum_ownership.rg`, `examples/generic_enum_clone.rg`, `examples/generic_enum_nested.rg`, and `examples/non_consuming_call.rg`.
- For move diagnostics, explicitly check later field access, method calls, double by-value passes, or second consuming receiver calls fail with `use of moved value`.
- For cleanup assertions, rely on exact stdout ordering/counts to prove reverse-order and exactly-once Drop behavior on normal exit, early `return`, nested `?`, overwrite, by-value return, and callee-exit cleanup.
- For generic-instantiation assertions, inspect generated Go for preserved concrete type arguments (for example `Container[T any]`, `OuterWrapped[int64]`, or `PairBoth[int64]`) in addition to runtime behavior.

## Flow Validator Guidance: rgoc CLI + generated Go artifacts + local HTTP probe

- Keep validation serial: callback validation shares one repo worktree, one `_build/`, the generated runtime binary path, and the reserved localhost port `3111`.
- Use the real CLI (`nix develop -c dune exec rgoc -- ...`) for both positive and negative fixtures, and keep `DUNE_BUILD_DIR` isolated under the assertion group's evidence directory.
- For positive callback assertions, compile representative fixtures under the evidence directory, then run `gofmt -d`, `go build`, and `go vet` on the generated Go before starting any server.
- For live callback checks, build the generated server as an evidence-local binary, start it with `--listen 127.0.0.1:3111`, and probe it with `curl` against the exact named-handler and anonymous-handler routes covered by the fixture.
- Repeated-request assertions must hit the same running process more than once per route so the evidence shows callbacks are long-lived and not consumed after first registration or first invocation.
- For negative callback assertions, verify both the user-facing diagnostic text and that no requested output file exists after the failed compile.
- Prefer the existing callback-focused coverage in `test/test_codegen.ml` and `test/test_typecheck.ml` as the source of representative fixture shapes for named handlers, zero-capture anonymous handlers, captured-lambda rejection, signature mismatches, and non-callable registration failures.
