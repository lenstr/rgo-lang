type compile_error =
  | Lex_error of { msg : string; line : int; col : int }
  | Parse_error of { msg : string; line : int; col : int }
  | Codegen_error of string

let compile_string ?(filename = "<input>") (source : string) :
    (string, compile_error) result =
  try
    let ast = Parse_driver.parse_string ~filename source in
    let go_src = Codegen.generate ast in
    Ok go_src
  with
  | Lexer.Lexer_error { msg; pos } ->
      Error (Lex_error { msg; line = pos.line; col = pos.col })
  | Parse_driver.Parse_error { msg; line; col } ->
      Error (Parse_error { msg; line; col })
  | Failure msg -> Error (Codegen_error msg)
