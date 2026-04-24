val parse_go_version_line : string -> (string, string) result
(** [parse_go_version_line line] parses a "go version" output line and checks
    whether the reported version is >= 1.26, which is required for trait [Self]
    support. Returns [Ok version_tag] on success or [Error reason] when the
    version is below minimum or unparseable. *)

val check_go_version_adequate : unit -> (string, string) result
(** [check_go_version_adequate ()] checks whether the Go toolchain version is >=
    1.26, which is required for trait [Self] support in generated code. Returns
    [Ok version_string] or [Error reason]. *)

type compile_error =
  | Lex_error of { msg : string; line : int; col : int }
  | Parse_error of { msg : string; line : int; col : int }
  | Resolve_error of { msg : string; line : int; col : int }
  | Typecheck_error of { msg : string; line : int; col : int }
  | Exhaust_error of { msg : string; line : int; col : int }
  | Codegen_error of string

val emit_tokens_string :
  ?filename:string -> string -> (string, compile_error) result
(** [emit_tokens_string ?filename source] lexes rgo source and returns a
    human-readable representation of the token stream. Useful for debugging the
    lexer. Returns [Ok tokens_string] on success, [Error e] on failure. *)

val emit_ast_string :
  ?filename:string -> string -> (string, compile_error) result
(** [emit_ast_string ?filename source] parses rgo source and returns a
    human-readable representation of the AST. Useful for debugging the parser.
    Returns [Ok ast_string] on success, [Error e] on failure. *)

val compile_string :
  ?filename:string -> string -> (string, compile_error) result
(** [compile_string ?filename source] compiles rgo source to Go source. Returns
    [Ok go_source] on success, [Error e] on failure. *)
