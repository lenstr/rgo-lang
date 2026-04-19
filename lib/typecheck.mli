(** Type checker for the rgo compiler. Validates types, performs local inference
    for [let] bindings, checks function call arguments, validates [?] on
    Result/Option, enforces mutability rules, and rejects invalid Self usage. *)

type typecheck_error = { msg : string; line : int; col : int }

exception Typecheck_error of typecheck_error

val typecheck_exn : Ast.program -> Ast.program
(** [typecheck_exn prog] type-checks the resolved AST. Returns the program on
    success, or raises [Typecheck_error] on failure. *)

val typecheck : Ast.program -> (Ast.program, string) result
(** [typecheck prog] type-checks the resolved AST. Returns [Ok prog] on success,
    or [Error msg] with a positional diagnostic on failure. *)
