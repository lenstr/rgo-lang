type compile_error =
  | Lex_error of { msg : string; line : int; col : int }
  | Parse_error of { msg : string; line : int; col : int }
  | Codegen_error of string

val compile_string :
  ?filename:string -> string -> (string, compile_error) result
(** [compile_string ?filename source] compiles rgo source to Go source. Returns
    [Ok go_source] on success, [Error e] on failure. *)
