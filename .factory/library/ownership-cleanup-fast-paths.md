# Ownership Cleanup Fast Paths

## Context

The codegen module (`lib/codegen.ml`) handles Drop cleanup for owned bindings when control flow exits a scope early (via `return`, `break`, `continue`, or block-final expressions). Specialized return forms like `Ok(x)`, `Err(x)`, `Some(x)`, and `None` have their own code paths distinct from the generic `return` statement.

## Key Invariant

**Guard suppression must happen before nested cleanup.** When ownership of a binding is being transferred (returned, consumed by a call), the binding's Drop guard must be set to `false` BEFORE `emit_all_nested_cleanup` runs. Otherwise, the nested cleanup will Drop the binding even though its ownership is being transferred, causing use-after-drop or double-cleanup bugs.

## Code Paths

### `emit_return_cleanup` (lib/codegen.ml)

Handles cleanup and guard suppression for generic return expressions. The order is:
1. `suppress_consumed_guards_inline` — suppress guards for identifiers consumed by value in the return expression
2. `emit_all_nested_cleanup` — clean up remaining Drop bindings (skips suppressed guards via `if guard { }` checks)

### Specialized return paths (both `gen_expr` and `gen_final_expr_as_return`)

For `Ok(arg)`, `Err(arg)`, `Some(arg)`, `None`, and bare identifier returns:
- If `arg` is a `Drop`-type binding (e.g., `Ok(r)` where `r` is a `Resource`), suppress `r`'s guard BEFORE calling `emit_return_cleanup`
- `emit_return_cleanup` then suppresses consumed guards and runs nested cleanup
- This two-step suppression ensures transferred bindings are not cleaned up

### `gen_block_with_return`

When a block's final expression generates a `return` statement, scope cleanup after the final expression is skipped. `gen_final_expr_as_return` handles all cleanup before the return, so `gen_block_with_return` should not emit redundant scope cleanup.

## Extracted Module

Ownership helpers live in `lib/codegen_ownership.ml` and `lib/codegen_ownership.mli`:
- `is_non_consuming_builtin` — builtin names that don't consume their arguments
- `collect_consumed_idents` — identifiers consumed by by-value calls in an expression
- `suppress_consumed_guards` / `suppress_consumed_guards_inline` — suppress Drop guards for consumed identifiers
- `suppress_move_guard` — suppress a single binding's guard
- `emit_drop_defer` / `emit_param_drop_defer` — emit defer-based cleanup
- `emit_overwrite_drop` — cleanup for mutable reassignment

## Test Coverage

Tests in `test/test_codegen_ownership.ml` cover:
- Normal scope exit cleanup (reverse-order exactly-once)
- Early `return` cleanup
- Nested `?` exit cleanup
- Specialized `return Ok/Err/Some/None` cleanup
- Final-expression `Ok/Err/Some/None` cleanup with guard suppression
- Final-expression identifier returns with nested cleanup
- Consuming call guard suppression in return expressions
- Loop `break`/`continue` cleanup
