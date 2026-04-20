---
name: compiler-worker
description: Implement and verify one compiler milestone feature in the rgo repository.
---

# Compiler Worker

NOTE: Startup and cleanup are handled by `worker-base`. This skill defines the WORK PROCEDURE.

## When to Use This Skill

Use for compiler implementation features in this mission: parser/AST, semantic analysis, ownership and move tracking, Drop/Copy/Clone behavior, exhaustiveness, code generation, CLI behavior, and related examples/tests.

## Required Skills

None.

## Work Procedure

1. Read `mission.md`, mission `AGENTS.md`, `.factory/services.yaml`, and relevant `.factory/library/*.md` files before changing code.
2. Inspect the existing repository area for the feature and match established file/module patterns before editing.
3. Write or extend tests first for the requested behavior. For compiler work this usually means unit tests, negative fixtures, snapshot/expect tests, or e2e fixtures. Make the new tests fail before implementing the feature.
4. Implement the smallest code change set needed to make the new tests pass while preserving PRD behavior.
5. Run the narrowest relevant validators during iteration, then run the mission-level validators from `.factory/services.yaml` before handoff.
6. For codegen, CLI, or ownership-sensitive features (Drop/Copy/Clone/moves), manually sanity-check at least one representative `.rg -> .go` flow and record the exact command/output path plus the observed runtime or diagnostic result in the handoff.
7. When a feature changes ownership-sensitive behavior, prefer one negative compile-path test and one positive generated-Go/runtime path so move-vs-copy-vs-clone vs cleanup timing are all observable.
8. Do not leave generated temp files or long-running processes behind. Clean up temporary outputs created for manual verification unless the feature explicitly adds fixtures.
9. In the handoff, be explicit about tests added, commands run, manual checks performed, and anything skipped or blocked.

## Example Handoff

```json
{
  "salientSummary": "Implemented enum exhaustiveness checking and wired negative fixtures into dune runtest. Added one positive fixture with wildcard coverage and one negative fixture missing a variant; all mission validators now pass.",
  "whatWasImplemented": "Added the Exhaust module pass over typed match expressions, integrated it into the driver before codegen, and extended the test harness with positive and negative enum coverage fixtures. The compiler now rejects non-exhaustive matches with a missing-variant diagnostic while accepting wildcard coverage.",
  "whatWasLeftUndone": "",
  "verification": {
    "commandsRun": [
      {
        "command": "nix develop -c dune runtest --no-buffer test/test_exhaust.exe",
        "exitCode": 0,
        "observation": "Positive and negative exhaustiveness fixtures passed."
      },
      {
        "command": "nix develop -c dune build",
        "exitCode": 0,
        "observation": "Repository builds cleanly after integrating the pass."
      },
      {
        "command": "nix develop -c dune runtest",
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
        "action": "Compiled a representative enum fixture with full coverage and then a fixture missing one variant.",
        "observed": "The covered fixture succeeded; the missing-variant fixture failed with a non-exhaustive-match diagnostic naming the uncovered case."
      }
    ]
  },
  "tests": {
    "added": [
      {
        "file": "test/test_exhaust.ml",
        "cases": [
          {
            "name": "wildcard match is accepted",
            "verifies": "A wildcard arm satisfies exhaustiveness for enum matches."
          },
          {
            "name": "missing enum variant is rejected",
            "verifies": "A non-exhaustive enum match produces a diagnostic containing the missing variant."
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
- The PRD is ambiguous for the requested milestone and the ambiguity affects observable compiler behavior.
- A required validation command from `.factory/services.yaml` is broken for reasons unrelated to the feature.
- Implementing the feature would require adding a new dependency or external service not already planned.
