# User Testing

Testing surface, required tools, and concurrency assumptions.

**What belongs here:** how validators should exercise the system, what artifacts to inspect, and resource notes.
**What does NOT belong here:** implementation details of compiler modules.

---

## Validation Surface

This mission exposes two validation surfaces only:

1. `rgoc` CLI invoked through `nix develop -c ...`
2. Generated Go artifacts produced from `.rg` fixtures and examples

Representative validation flows:
- compile success/failure through the real CLI
- verify failed compilations do not leave misleading output files behind
- compile `.rg` input to `.go` via `rgoc input.rg -o output.go`
- run `gofmt -d`, `go build`, `go vet`, and `go run` on generated output
- use exact stdout ordering/counts to validate move/copy/clone/drop behavior
- include both direct `?` flows and nested `?` flows when validating cleanup on early exit

## Validation Concurrency

- Surface: `rgoc` CLI / generated Go artifacts
- Max concurrent validators: 1
- Rationale: validation uses shared local build outputs and the same repository working tree, so serial execution avoids interference while remaining fast for this compiler project.

## Flow Validator Guidance: CLI

- Stay inside the repository root and use `nix develop -c ...` for all commands.
- Do not start servers or bind ports; this mission only validates the compiler CLI and generated Go artifacts.
- Prefer temporary output files or dedicated tracked fixtures when checking generated Go behavior.
- Treat `_build/`, temporary generated Go files, and repo-local test artifacts as shared mutable state; run validations serially unless explicit isolated copies are provisioned.
- For Drop/Copy/Clone, prefer fixtures whose stdout makes ownership transfer and cleanup timing obvious (distinct markers per binding or path).
