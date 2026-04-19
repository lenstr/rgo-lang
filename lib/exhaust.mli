(** Exhaustiveness checking for enum [match] expressions. Validates that every
    variant of the scrutinee enum is covered by at least one arm, or that a
    wildcard/catch-all pattern exists. *)

type exhaust_error = { msg : string; line : int; col : int }

exception Exhaust_error of exhaust_error

val check_exn : Ast.program -> Ast.program
(** [check_exn prog] checks all match expressions in [prog] for exhaustiveness.
    Returns [prog] unchanged on success, or raises [Exhaust_error] when a
    non-exhaustive match is found. *)

val check : Ast.program -> (Ast.program, string) result
(** [check prog] is the non-raising variant. Returns [Ok prog] on success, or
    [Error msg] with a positional diagnostic on failure. *)
