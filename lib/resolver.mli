type resolve_error = { msg : string; line : int; col : int }

exception Resolve_error of resolve_error

type import_error_kind =
  | Malformed_import of string
  | Unsupported_external_package of string
  | Import_alias_collision of string * Ast.span
  | Missing_import of string

exception Import_error of { kind : import_error_kind; span : Ast.span }

val format_import_error : import_error_kind -> string
val alias_for_path : string list -> string option

val resolve_exn : Ast.program -> Ast.program
(** [resolve_exn prog] performs name resolution on the AST. Returns the program
    when all names resolve, or raises [Resolve_error] on failure. *)

val resolve : Ast.program -> (Ast.program, string) result
(** [resolve prog] performs name resolution on the AST. Returns [Ok prog] when
    all names resolve successfully, or [Error msg] when an undefined name,
    undefined type, undefined variant, undefined field, or duplicate definition
    is detected. *)
