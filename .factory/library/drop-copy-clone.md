# Drop Copy Clone

Mission-specific semantic boundaries and invariants for workers.

**What belongs here:** the agreed MVP boundary, semantic guardrails, and implementation-shaping invariants for `Drop`, `Copy`, and `Clone`.
**What does NOT belong here:** exact code patches or command lines.

---

## Agreed MVP boundary

- Whole named bindings only: no partial moves from fields or enum payloads.
- No full borrow checker or lifetime analysis.
- `Drop` is for user-defined nominal types in this mission (`struct`, `enum`, including generic nominal types).
- `Copy` is Rust-like in intent: eligible values may be reused after assignment, by-value call, or return because they are copied rather than moved.
- `Clone` is explicit and Rust-like: `clone()` is the user-facing duplication path for non-`Copy` values.
- `Drop` types cannot also be `Copy`.

## Binding-state invariants

Workers should preserve a simple whole-binding state model:

- live and owned
- moved and unusable
- overwritten (old value cleaned up exactly once; binding now refers to the new value)

The user-visible contract is more important than the internal representation, but the implementation should keep those states clear.

## Cleanup invariants

- Clean up only still-live owned bindings.
- Use reverse declaration order for normal scope exit.
- Preserve cleanup on supported early exits (`return`, `?`, and other explicitly in-scope control-flow exits).
- Never double-drop a moved or already-overwritten value.

## Generic invariants

- Preserve concrete generic instantiations across move/copy/clone/drop paths.
- Avoid collapsing instantiated nominal types to bare nominal names in semantic analysis or codegen.
- When Copy/Clone depend on generic bounds, enforce those bounds explicitly.

## Validation hints

Representative fixtures should cover:
- assignment and by-value call move sites
- clone as an escape hatch at a real move site
- reverse-order cleanup on scope exit
- cleanup on `return` and `?`
- overwrite cleanup for mutable bindings
- generic struct and generic enum paths
