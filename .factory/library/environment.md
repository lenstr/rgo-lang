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
- `build = nix develop -c dune build -j 16`
- `test = nix develop -c dune runtest -j 16`
- `lint = nix develop -c dune build @fmt`
- `typecheck = nix develop -c dune build -j 16`
- `cli_version = nix develop -c dune exec rgoc -- --version`
- `go_version = nix develop -c go version`

The flake currently provides Go 1.26+, OCaml 5.x, dune, menhir, sedlex, ppx_deriving, alcotest, ppx_expect, ocamlformat, ocaml-lsp, and bisect_ppx.

## External dependencies

This mission has no external credentials, APIs, databases, or third-party services.
The only runtime dependency added by the mission is the Go stdlib surface exercised through generated Go binaries.

## Mission-specific setup notes

- The interop-first mission remains repo-local and stdlib-first.
- The canonical representative CRUD validation fixture is `.factory/runtime/interop-crud.rg`; automated coverage for the live HTTP validation flow should reuse that tracked source instead of duplicating inline CRUD programs.
- Validators and workers should compile the representative HTTP fixture into `./.factory/runtime/http-validation-server` for live HTTP checks.
- The HTTP validation server should bind `127.0.0.1:3111` only.
- Dune ignores hidden top-level directories by default. If tracked test fixtures under `.factory/` must be available during `dune` runs, the repo needs a root `dune` file that opts `.factory` in with `(dirs :standard .factory)`, and affected test stanzas still need explicit fixture `deps`.
- Temporary generated Go files should be written under `.factory/runtime/` or another disposable path and cleaned up unless promoted to tracked fixtures.
