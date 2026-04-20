# Environment

Environment variables, external dependencies, and setup notes.

**What belongs here:** required env vars, external toolchain assumptions, setup notes.
**What does NOT belong here:** service commands or ports.

---

## Toolchain

All commands must run through the repository flake:
- `nix develop -c dune ...`
- `nix develop -c go ...`
- `nix develop -c ocaml ...`

Validation entrypoints are standardized in `.factory/services.yaml`:
- `build = nix develop -c dune build`
- `test = nix develop -c dune runtest`
- `lint = nix develop -c dune build @fmt`
- `typecheck = nix develop -c dune build`
- `cli_version = nix develop -c dune exec rgoc -- --version`

The flake currently provides Go 1.26+, OCaml 5.x, dune, menhir, sedlex, ppx_deriving, alcotest, ppx_expect, ocamlformat, ocaml-lsp, and bisect_ppx.

## External dependencies

This mission has no external credentials, APIs, databases, or background services.

## Mission-specific setup notes

- The Drop/Copy/Clone mission adds no new runtime services.
- Validation should stay inside the repo root and use the flake toolchain only.
- Temporary generated Go files should be written outside the repo or cleaned up immediately unless promoted to tracked fixtures.
