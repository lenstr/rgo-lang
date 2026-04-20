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
- compile the representative HTTP fixture to `./.factory/runtime/http-validation-server`
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
- Bind the generated HTTP fixture to `127.0.0.1:3111` only.
- Prefer `curl` for HTTP assertions and capture full response status/body for create/read/error paths.
- Treat `_build/`, `.factory/runtime/`, temporary generated Go files, and the localhost validation port as shared mutable state; run validations serially unless explicit isolated copies are provisioned.
- For ownership-sensitive flows, prefer fixtures whose stdout makes ownership transfer and cleanup timing obvious (distinct markers per binding or path).
