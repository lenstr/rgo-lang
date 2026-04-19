type error = { msg : string; pos : Token.pos }

exception Lexer_error of error

val tokenize : string -> Token.located list
(** [tokenize source] lexes the entire source string into a list of located
    tokens. Raises [Lexer_error] on invalid input. The final token is always
    [Token.Eof]. *)
