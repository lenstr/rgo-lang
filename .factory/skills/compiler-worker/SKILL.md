---
name: compiler-worker
description: Implement and verify one compiler milestone feature in the rgo repository.
---

# Compiler Worker

NOTE: Startup and cleanup are handled by `worker-base`. This skill defines the WORK PROCEDURE.

## When to Use This Skill

Use for compiler implementation features in this mission: import syntax and namespace resolution, stdlib naming translation, interop metadata, ownership and cleanup planning, callback/lambda support, `net/http` lowering, generated Go validation, and related fixtures/tests.

## Required Skills

None.

## Work Procedure

1. Read `mission.md`, mission `AGENTS.md`, `.factory/services.yaml`, `validation-contract.md`, and the relevant `.factory/library/*.md` files before changing code.
2. Match the existing repository structure and extend the smallest possible parser / resolver / typecheck / planning / codegen surface for the feature.
3. Write tests first. Prefer a focused positive fixture and a focused negative fixture for each new compiler behavior. For interop work, add at least one real `.rg -> .go` acceptance check when the feature affects codegen.
4. Implement only the behavior needed for the current feature and mission slice; do not broaden the supported Go surface beyond the contracted `net/http` subset unless the feature explicitly requires it.
5. For naming-bridge work, verify both the accepted rgo-facing spelling and the rejected wrong-case spelling.
6. For ownership-sensitive work, verify one negative compile path and one positive runtime path so move/copy/clone/drop behavior is externally observable.
7. For callback or HTTP-facing work, manually exercise the generated artifact on `127.0.0.1:3111` when the feature makes that possible. Capture the exact compile/build/run/curl commands and what each request proved.
8. Run the narrowest relevant validators during iteration, then run the mission-level commands from `.factory/services.yaml` before handoff.
9. Do not leave generated temp files or long-running processes behind. Put disposable runtime artifacts under `.factory/runtime/` and clean them up unless the feature explicitly adds tracked fixtures.
10. In the handoff, be explicit about tests added, commands run, runtime probes performed, and any deferred ambiguity or unsupported cases.

## Example Handoff

```json
{
  "salientSummary": "Implemented `use net::http` parsing plus package-qualified stdlib name resolution and added one positive fixture plus two negative casing/import fixtures. The new fixture now lowers to real `import \"net/http\"` Go and the repo validators stay green.",
  "whatWasImplemented": "Added AST/parser/resolver support for `use net::http` and package-qualified stdlib references, wired the naming bridge for `http::listen_and_serve` and `http::Request`, and extended the test suite with positive and negative fixtures covering qualified imports, missing imports, and wrong-case stdlib references.",
  "whatWasLeftUndone": "Receiver-member naming for handler bodies is still pending in a later feature.",
  "verification": {
    "commandsRun": [
      {
        "command": "nix develop -c dune runtest -j 16 test/test_parser.exe test/test_resolver.exe test/test_codegen.exe",
        "exitCode": 0,
        "observation": "New import and naming fixtures passed."
      },
      {
        "command": "nix develop -c dune build -j 16",
        "exitCode": 0,
        "observation": "Repository builds cleanly after the interop parser/resolver changes."
      },
      {
        "command": "nix develop -c dune runtest -j 16",
        "exitCode": 0,
        "observation": "Full test suite passed."
      },
      {
        "command": "nix develop -c dune build @fmt",
        "exitCode": 0,
        "observation": "Formatting target stayed clean."
      }
    ],
    "interactiveChecks": [
      {
        "action": "Compiled a positive `use net::http` fixture to `.factory/runtime/http-import.go` and inspected the lowered import and symbol spellings.",
        "observed": "Generated Go contained `import \"net/http\"`, `http.ListenAndServe`, and `*http.Request` with no manual patching required."
      }
    ]
  },
  "tests": {
    "added": [
      {
        "file": "test/test_typecheck.ml",
        "cases": [
          {
            "name": "use net::http enables qualified stdlib symbols",
            "verifies": "Package-qualified stdlib names resolve after import and lower to real Go imports."
          },
          {
            "name": "wrong-case stdlib callable is rejected",
            "verifies": "Go-cased callable names fail in the rgo-facing naming bridge."
          }
        ]
      }
    ]
  },
  "discoveredIssues": []
}
```

## When to Return to Orchestrator

- The feature depends on a prior compiler phase that is not implemented or is too broken to proceed safely.
- The feature requires expanding beyond the approved `net/http` mission slice or beyond zero-capture anonymous handlers.
- A required validation command or the reserved localhost validation port (`127.0.0.1:3111`) is unavailable for reasons unrelated to the feature.
- Implementing the feature would require a new dependency, service, or external package mechanism not already planned.
