type error = { msg : string; pos : Token.pos }

exception Lexer_error of error

(* Position tracking state *)
type state = { mutable line : int; mutable line_start_offset : int }

let make_state () = { line = 1; line_start_offset = 0 }

let current_pos state (buf : Sedlexing.lexbuf) =
  let ofs = Sedlexing.lexeme_start buf in
  let col = ofs - state.line_start_offset + 1 in
  Token.{ line = state.line; col; offset = ofs }

let end_pos state (buf : Sedlexing.lexbuf) =
  let ofs = Sedlexing.lexeme_end buf in
  let col = ofs - state.line_start_offset + 1 in
  Token.{ line = state.line; col; offset = ofs }

let make_span state buf =
  Token.{ start = current_pos state buf; stop = end_pos state buf }

let keyword_or_ident s =
  match s with
  | "fn" -> Token.Fn
  | "let" -> Token.Let
  | "mut" -> Token.Mut
  | "if" -> Token.If
  | "else" -> Token.Else
  | "match" -> Token.Match
  | "return" -> Token.Return
  | "struct" -> Token.Struct
  | "enum" -> Token.Enum
  | "for" -> Token.For
  | "while" -> Token.While
  | "loop" -> Token.Loop
  | "break" -> Token.Break
  | "continue" -> Token.Continue
  | "in" -> Token.In
  | "as" -> Token.As
  | "pub" -> Token.Pub
  | "use" -> Token.Use
  | "mod" -> Token.Mod
  | "impl" -> Token.Impl
  | "trait" -> Token.Trait
  | "true" -> Token.True
  | "false" -> Token.False
  | "Self" -> Token.SelfType
  | "self" -> Token.SelfValue
  | "i8" -> Token.I8
  | "i16" -> Token.I16
  | "i32" -> Token.I32
  | "i64" -> Token.I64
  | "u8" -> Token.U8
  | "u16" -> Token.U16
  | "u32" -> Token.U32
  | "u64" -> Token.U64
  | "f32" -> Token.F32
  | "f64" -> Token.F64
  | "bool" -> Token.Bool
  | "str" -> Token.Str
  | "String" -> Token.StringType
  | "Option" -> Token.OptionType
  | "Result" -> Token.ResultType
  | "Vec" -> Token.VecType
  | "HashMap" -> Token.HashMapType
  | "_" -> Token.Underscore
  | _ -> Token.Ident s

(* Skip nested block comments. Returns with buf positioned after the closing */.
   Handles nesting: /* ... /* ... */ ... */ *)
let rec skip_block_comment state buf depth =
  match%sedlex buf with
  | "/*" -> skip_block_comment state buf (depth + 1)
  | "*/" -> if depth > 1 then skip_block_comment state buf (depth - 1)
  | '\n' ->
      state.line <- state.line + 1;
      state.line_start_offset <- Sedlexing.lexeme_end buf;
      skip_block_comment state buf depth
  | any -> skip_block_comment state buf depth
  | eof ->
      raise
        (Lexer_error
           { msg = "unterminated block comment"; pos = current_pos state buf })
  | _ -> assert false

(* Lex a string literal. Called after the opening quote is consumed. *)
let lex_string state buf =
  let b = Buffer.create 64 in
  let start_pos = current_pos state buf in
  let rec go () =
    match%sedlex buf with
    | '"' -> Buffer.contents b
    | "\\n" ->
        Buffer.add_char b '\n';
        go ()
    | "\\t" ->
        Buffer.add_char b '\t';
        go ()
    | "\\r" ->
        Buffer.add_char b '\r';
        go ()
    | "\\\\" ->
        Buffer.add_char b '\\';
        go ()
    | "\\\"" ->
        Buffer.add_char b '"';
        go ()
    | "\\0" ->
        Buffer.add_char b '\x00';
        go ()
    | '\n' ->
        state.line <- state.line + 1;
        state.line_start_offset <- Sedlexing.lexeme_end buf;
        Buffer.add_char b '\n';
        go ()
    | Sub (any, ('"' | '\\' | '\n')) ->
        let s = Sedlexing.Utf8.lexeme buf in
        Buffer.add_string b s;
        go ()
    | eof ->
        raise
          (Lexer_error { msg = "unterminated string literal"; pos = start_pos })
    | _ -> assert false
  in
  go ()

let rec token state buf =
  match%sedlex buf with
  (* Whitespace - skip, but track newlines *)
  | Plus (Chars " \t\r") -> token state buf
  | '\n' ->
      state.line <- state.line + 1;
      state.line_start_offset <- Sedlexing.lexeme_end buf;
      token state buf
  (* Line comments *)
  | "//", Star (Sub (any, '\n')) -> token state buf
  (* Block comments *)
  | "/*" ->
      skip_block_comment state buf 1;
      token state buf
  (* String literals *)
  | '"' ->
      let start = current_pos state buf in
      let s = lex_string state buf in
      let stop = end_pos state buf in
      Token.{ token = StringLit s; span = { start; stop } }
  (* Malformed prefixed integers - must come before valid hex/binary patterns
     to catch e.g. 0x_ and 0b_ before they fall through as 0 + identifier *)
  | ("0x" | "0X"), Plus '_' ->
      raise
        (Lexer_error
           {
             msg = "malformed hex literal: expected hex digit after prefix";
             pos = current_pos state buf;
           })
  | ("0b" | "0B"), Plus '_' ->
      raise
        (Lexer_error
           {
             msg =
               "malformed binary literal: expected binary digit after prefix";
             pos = current_pos state buf;
           })
  (* Numeric literals - hex: allow leading underscores but require >= 1 hex digit *)
  | ( ("0x" | "0X"),
      Star '_',
      ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F'),
      Star ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | '_') ) ->
      let s = Sedlexing.Utf8.lexeme buf in
      Token.{ token = IntLit s; span = make_span state buf }
  (* Numeric literals - binary: allow leading underscores but require >= 1 binary digit *)
  | ("0b" | "0B"), Star '_', ('0' | '1'), Star ('0' | '1' | '_') ->
      let s = Sedlexing.Utf8.lexeme buf in
      Token.{ token = IntLit s; span = make_span state buf }
  (* Float literals - must come before decimal integers *)
  | '0' .. '9', Star ('0' .. '9' | '_'), '.', '0' .. '9', Star ('0' .. '9' | '_')
    ->
      let s = Sedlexing.Utf8.lexeme buf in
      Token.{ token = FloatLit s; span = make_span state buf }
  | ( '0' .. '9',
      Star ('0' .. '9' | '_'),
      Opt ('.', '0' .. '9', Star ('0' .. '9' | '_')),
      ('e' | 'E'),
      Opt ('+' | '-'),
      '0' .. '9',
      Star ('0' .. '9' | '_') ) ->
      let s = Sedlexing.Utf8.lexeme buf in
      Token.{ token = FloatLit s; span = make_span state buf }
  (* Decimal integer literals - must start with a digit *)
  | '0' .. '9', Star ('0' .. '9' | '_') ->
      let s = Sedlexing.Utf8.lexeme buf in
      Token.{ token = IntLit s; span = make_span state buf }
  (* Multi-char operators - must come before single-char *)
  | "==" -> Token.{ token = EqEq; span = make_span state buf }
  | "!=" -> Token.{ token = NotEq; span = make_span state buf }
  | "<=" -> Token.{ token = LtEq; span = make_span state buf }
  | ">=" -> Token.{ token = GtEq; span = make_span state buf }
  | "&&" -> Token.{ token = AmpAmp; span = make_span state buf }
  | "||" -> Token.{ token = PipePipe; span = make_span state buf }
  | "+=" -> Token.{ token = PlusEq; span = make_span state buf }
  | "-=" -> Token.{ token = MinusEq; span = make_span state buf }
  | "*=" -> Token.{ token = StarEq; span = make_span state buf }
  | "/=" -> Token.{ token = SlashEq; span = make_span state buf }
  | "->" -> Token.{ token = Arrow; span = make_span state buf }
  | "=>" -> Token.{ token = FatArrow; span = make_span state buf }
  | "::" -> Token.{ token = ColonColon; span = make_span state buf }
  (* Single-char operators and punctuation *)
  | '+' -> Token.{ token = Plus; span = make_span state buf }
  | '-' -> Token.{ token = Minus; span = make_span state buf }
  | '*' -> Token.{ token = Star; span = make_span state buf }
  | '/' -> Token.{ token = Slash; span = make_span state buf }
  | '%' -> Token.{ token = Percent; span = make_span state buf }
  | '<' -> Token.{ token = Lt; span = make_span state buf }
  | '>' -> Token.{ token = Gt; span = make_span state buf }
  | '!' -> Token.{ token = Bang; span = make_span state buf }
  | '=' -> Token.{ token = Eq; span = make_span state buf }
  | '.' -> Token.{ token = Dot; span = make_span state buf }
  | ',' -> Token.{ token = Comma; span = make_span state buf }
  | ';' -> Token.{ token = Semi; span = make_span state buf }
  | ':' -> Token.{ token = Colon; span = make_span state buf }
  | '?' -> Token.{ token = Question; span = make_span state buf }
  | '&' -> Token.{ token = Amp; span = make_span state buf }
  | '(' -> Token.{ token = LParen; span = make_span state buf }
  | ')' -> Token.{ token = RParen; span = make_span state buf }
  | '{' -> Token.{ token = LBrace; span = make_span state buf }
  | '}' -> Token.{ token = RBrace; span = make_span state buf }
  | '[' -> Token.{ token = LBracket; span = make_span state buf }
  | ']' -> Token.{ token = RBracket; span = make_span state buf }
  (* Identifiers and keywords - Unicode-aware *)
  | ( ('_' | lu | ll | lt | lm | lo),
      Star ('_' | lu | ll | lt | lm | lo | nd | mn | mc) ) ->
      let s = Sedlexing.Utf8.lexeme buf in
      let tok = keyword_or_ident s in
      Token.{ token = tok; span = make_span state buf }
  (* EOF *)
  | eof -> Token.{ token = Eof; span = make_span state buf }
  (* Invalid character *)
  | any ->
      let s = Sedlexing.Utf8.lexeme buf in
      raise
        (Lexer_error
           {
             msg = Printf.sprintf "unexpected character: %s" s;
             pos = current_pos state buf;
           })
  | _ -> assert false

let tokenize source =
  let buf = Sedlexing.Utf8.from_string source in
  let state = make_state () in
  let rec go acc =
    let tok = token state buf in
    let acc = tok :: acc in
    match tok.token with Token.Eof -> List.rev acc | _ -> go acc
  in
  go []
