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

The flake currently provides Go 1.26+, OCaml 5.x, dune, menhir, sedlex, ppx_deriving, alcotest, ppx_expect, ocamlformat, ocaml-lsp, and bisect_ppx.

## External dependencies

This mission has no external credentials, APIs, databases, or background services.
