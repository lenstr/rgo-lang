# Arbitrary-Depth Built-in Pattern Codegen

## Context

The codegen module (`lib/codegen.ml`) handles pattern matching on built-in `Option` and `Result` types. For nested patterns like `Option::Some(Option::Some(v))` or `Result::Ok(Option::Some(v))`, the compiler must generate recursive if/else chains that unwrap each level.

## Previous Behavior

Before this feature, nested built-in pattern codegen only handled one level of nesting. Patterns deeper than one level (e.g., `Option::Some(Option::Some(Option::Some(v)))`) would fall through to `panic("unsupported nested pattern")` in the generated Go code. The wildcard/default fallback handling also only worked at the first nesting level.

## What Changed

Four codegen functions now support arbitrary-depth recursion for built-in patterns:

1. `gen_nested_option_match_stmt` — Statement-context Option matching
2. `gen_nested_option_match_expr` — Expression-context Option matching
3. `gen_nested_result_inner_match_stmt` — Statement-context Result inner matching
4. `gen_nested_result_inner_match_expr` — Expression-context Result inner matching

Each function now:

- Collects ALL arms for a variant (not just the first), so deeper patterns at the same level aren't lost
- Detects when an inner pattern is itself a nested built-in (`Option` or `Result`)
- Peels one level from all matching arms and recurses into the appropriate handler
- Preserves wildcard/default fallback at every recursion depth

Three helper functions were added:

- `peel_one_builtin_level` — Strips the outer variant from each arm, producing arms at the next nesting level
- `list_head_opt` — Safe head of list (replaces `List.hd_opt` which isn't available in OCaml 5.x stdlib)
- `has_deeper_builtin_nesting` — Detective helper (currently unused but available)

## Key Design Decision

The recursive functions pass the **current inner struct variable** (e.g., `inner_name`) to the recursive call rather than a pre-unwrapped value. This is because each function already unwraps its input variable (`inner_name := *outer_var` or `inner_name := outer_var.value`), so the recursive call reuses the same pattern correctly. Passing a pre-unwrapped `deeper_var` caused double-unwrapping issues where the generated Go tried to access `.value` on a pointer type instead of a struct type.

## Test Coverage

New tests in `test/test_codegen.ml`:
- `test_option_deep_nested_3_levels` — 3-level `Option<Option<Option<i64>>>` matching
- `test_option_deep_nested_3_levels_default_fallback` — Default fallback at depth 3
- `test_option_deep_nested_3_levels_outer_none` — Outer None arm at depth 3

Existing tests (all still passing):
- 2-level nested Option/Result patterns with wildcards and default fallbacks
- Ownership test for nested built-in Copy-payload patterns

## Generated Go Behavior

For `Option<Option<Option<i64>>>` matching `Some(Some(Some(v)))`:
```go
if __match_opt_0 := a; __match_opt_0 != nil {
    __inner_opt_1 := *__match_opt_0
    if __inner_opt_1.some {
        __inner_opt_2 := __inner_opt_1.value
        if __inner_opt_2 != nil {
            v := *__inner_opt_2
            // body
        } else {
            // inner-inner None arm
        }
    } else {
        // inner None arm
    }
} else {
    // outer None arm
}
```

No `panic("unsupported nested pattern")` or `panic("unreachable")` for legal accepted nested patterns.
