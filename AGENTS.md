# AGENTS.md

## Development environment

For all work in this repository, use the Nix flake environment from `flake.nix`.

### Required workflow

- Run project commands through `nix develop -c ...`
- Prefer flake-provided tools over globally installed system tools
- Do **not** install project dependencies with `yay`, `pacman`, or global `opam` unless the user explicitly asks
- Do **not** assume the host environment matches the project toolchain

### Examples

```bash
nix develop -c go version
nix develop -c ocaml -version
nix develop -c dune build
nix develop -c dune runtest
nix develop -c menhir --version
nix develop -c ocamlformat --version
```

For longer sessions, entering the shell is fine:

```bash
nix develop
```

## Toolchain source of truth

The source of truth for the development environment is:

- `flake.nix`
- `flake.lock`

Agents should treat the flake as authoritative for:

- Go
- OCaml
- dune
- menhir
- sedlex
- ppx_deriving
- alcotest
- ppx_expect
- ocamlformat
- ocaml-lsp
- bisect_ppx
- supporting build tools

## Project note

This project targets:

- OCaml 5.x for the compiler implementation
- Go 1.26+ as the compilation target

If a command behaves differently inside and outside Nix, prefer the behavior inside `nix develop`.
