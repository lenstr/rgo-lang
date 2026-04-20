## Validation Concurrency

- Surface: `rgoc` CLI / generated Go artifacts
- Max concurrent validators: 1
- Rationale: validation uses shared local build outputs and the same repository working tree, so serial execution avoids interference while remaining fast for this compiler project.

## Flow Validator Guidance: CLI

- Stay inside the repository root and use `nix develop -c ...` for all commands.
- Do not start servers or bind ports; this mission only validates the compiler CLI and generated Go artifacts.
- Prefer temporary output files or existing test fixtures when checking generated Go behavior.
- Treat `_build/`, temporary generated Go files, and repo-local test artifacts as shared mutable state; run validations serially unless explicit isolated copies are provisioned.