type resolve_error = { msg : string; line : int; col : int }

exception Resolve_error of resolve_error

val resolve_exn : Ast.program -> Ast.program
(** [resolve_exn prog] performs name resolution on the AST. Returns the program
    when all names resolve, or raises [Resolve_error] on failure. *)

val resolve : Ast.program -> (Ast.program, string) result
(** [resolve prog] performs name resolution on the AST. Returns [Ok prog] when
    all names resolve successfully, or [Error msg] when an undefined name,
    undefined type, undefined variant, undefined field, or duplicate definition
    is detected. *)
