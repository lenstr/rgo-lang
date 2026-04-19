type parse_error = { msg : string; line : int; col : int }

exception Parse_error of parse_error

val parse_string : ?filename:string -> string -> Ast.program
(** [parse_string ?filename source] parses the given source string into an AST.
    Raises [Parse_error] on syntax errors and [Lexer.Lexer_error] on lexical
    errors. *)
