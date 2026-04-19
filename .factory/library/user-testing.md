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
- build/test/fmt smoke checks for skeleton milestones
- compile `.rg` input to `.go` via `rgoc input.rg -o output.go`
- compare generated Go against expected snapshots when required
- run `gofmt -d`, `go build`, `go run`, and `go vet` on generated output for codegen milestones

## Validation Concurrency

- Surface: CLI/compiler validation
  - Max concurrent validators: 5
  - Rationale: this is a lightweight local CLI workload on a machine with 32 CPUs and ~61.7 GB RAM, and no browser/app instances are involved.

- Surface: generated Go validation
  - Max concurrent validators: 5
  - Rationale: `go build`/`go run`/`go vet` on generated fixtures are moderate local processes with ample headroom on the current machine.

Use lower concurrency if later milestones reveal unexpectedly heavy fixture suites or subprocess fan-out.
