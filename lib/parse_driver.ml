type parse_error = { msg : string; line : int; col : int }

exception Parse_error of parse_error

(* Bridge between sedlex (our lexer) and Menhir (standard API).
   We pre-lex with sedlex, then feed tokens to Menhir via a
   Lexing.lexbuf + token function pair. *)

let token_to_parser (tok : Token.located) : Parser.token =
  match tok.token with
  | Token.Fn -> Parser.FN
  | Token.Let -> Parser.LET
  | Token.Mut -> Parser.MUT
  | Token.If -> Parser.IF
  | Token.Else -> Parser.ELSE
  | Token.Match -> Parser.MATCH
  | Token.Return -> Parser.RETURN
  | Token.Struct -> Parser.STRUCT
  | Token.Enum -> Parser.ENUM
  | Token.For -> Parser.FOR
  | Token.While -> Parser.WHILE
  | Token.Loop -> Parser.LOOP
  | Token.Break -> Parser.BREAK
  | Token.Continue -> Parser.CONTINUE
  | Token.In -> Parser.IN
  | Token.As -> Parser.AS
  | Token.Pub -> Parser.PUB
  | Token.Use -> Parser.USE
  | Token.Mod -> Parser.MOD
  | Token.Impl -> Parser.IMPL
  | Token.Trait -> Parser.TRAIT
  | Token.SelfType -> Parser.SELF_TYPE
  | Token.SelfValue -> Parser.SELF_VALUE
  | Token.True -> Parser.TRUE
  | Token.False -> Parser.FALSE
  | Token.I8 -> Parser.I8
  | Token.I16 -> Parser.I16
  | Token.I32 -> Parser.I32
  | Token.I64 -> Parser.I64
  | Token.U8 -> Parser.U8
  | Token.U16 -> Parser.U16
  | Token.U32 -> Parser.U32
  | Token.U64 -> Parser.U64
  | Token.F32 -> Parser.F32
  | Token.F64 -> Parser.F64
  | Token.Bool -> Parser.BOOL_TYPE
  | Token.Str -> Parser.STR
  | Token.StringType -> Parser.STRING_TYPE
  | Token.OptionType -> Parser.OPTION_TYPE
  | Token.ResultType -> Parser.RESULT_TYPE
  | Token.VecType -> Parser.VEC_TYPE
  | Token.HashMapType -> Parser.HASHMAP_TYPE
  | Token.IntLit s -> Parser.INT_LIT s
  | Token.FloatLit s -> Parser.FLOAT_LIT s
  | Token.StringLit s -> Parser.STRING_LIT s
  | Token.Ident s -> Parser.IDENT s
  | Token.Plus -> Parser.PLUS
  | Token.Minus -> Parser.MINUS
  | Token.Star -> Parser.STAR
  | Token.Slash -> Parser.SLASH
  | Token.Percent -> Parser.PERCENT
  | Token.EqEq -> Parser.EQEQ
  | Token.NotEq -> Parser.NEQ
  | Token.Lt -> Parser.LT
  | Token.Gt -> Parser.GT
  | Token.LtEq -> Parser.LTEQ
  | Token.GtEq -> Parser.GTEQ
  | Token.AmpAmp -> Parser.AMPAMP
  | Token.PipePipe -> Parser.PIPEPIPE
  | Token.Bang -> Parser.BANG
  | Token.Eq -> Parser.EQ
  | Token.PlusEq -> Parser.PLUSEQ
  | Token.MinusEq -> Parser.MINUSEQ
  | Token.StarEq -> Parser.STAREQ
  | Token.SlashEq -> Parser.SLASHEQ
  | Token.Arrow -> Parser.ARROW
  | Token.FatArrow -> Parser.FAT_ARROW
  | Token.ColonColon -> Parser.COLON_COLON
  | Token.Dot -> Parser.DOT
  | Token.Comma -> Parser.COMMA
  | Token.Semi -> Parser.SEMI
  | Token.Colon -> Parser.COLON
  | Token.Question -> Parser.QUESTION
  | Token.Amp -> Parser.AMP
  | Token.Pipe -> Parser.PIPE
  | Token.LParen -> Parser.LPAREN
  | Token.RParen -> Parser.RPAREN
  | Token.LBrace -> Parser.LBRACE
  | Token.RBrace -> Parser.RBRACE
  | Token.LBracket -> Parser.LBRACKET
  | Token.RBracket -> Parser.RBRACKET
  | Token.Underscore -> Parser.UNDERSCORE
  | Token.Eof -> Parser.EOF

let parse_string ?(filename = "<input>") (source : string) : Ast.program =
  let tokens = Lexer.tokenize source in
  let tokens_arr = Array.of_list tokens in
  let pos = ref 0 in
  (* Create a dummy Lexing.lexbuf that we update positions on *)
  let lexbuf = Lexing.from_string "" in
  lexbuf.lex_curr_p <-
    { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  let next_token _lb =
    let i = !pos in
    let tok =
      if i >= Array.length tokens_arr then
        Token.
          {
            token = Eof;
            span =
              {
                start = { line = 1; col = 1; offset = 0 };
                stop = { line = 1; col = 1; offset = 0 };
              };
          }
      else (
        pos := i + 1;
        tokens_arr.(i))
    in
    (* Update lexbuf positions so Menhir can report positions *)
    let s = tok.span.start in
    let e = tok.span.stop in
    lexbuf.lex_start_p <-
      {
        pos_fname = filename;
        pos_lnum = s.line;
        pos_bol = s.offset - s.col + 1;
        pos_cnum = s.offset;
      };
    lexbuf.lex_curr_p <-
      {
        pos_fname = filename;
        pos_lnum = e.line;
        pos_bol = e.offset - e.col + 1;
        pos_cnum = e.offset;
      };
    token_to_parser tok
  in
  try Parser.program next_token lexbuf
  with Parser.Error ->
    let p = lexbuf.lex_start_p in
    raise
      (Parse_error
         {
           msg = "syntax error";
           line = p.pos_lnum;
           col = p.pos_cnum - p.pos_bol + 1;
         })
