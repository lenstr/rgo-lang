open Rgo

let string_starts_with ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let token_testable : Token.t Alcotest.testable =
  Alcotest.testable (fun fmt t -> Format.fprintf fmt "%s" (Token.show t)) ( = )

let tokens_testable : Token.t list Alcotest.testable =
  Alcotest.list token_testable

let lex_tokens src =
  let located = Lexer.tokenize src in
  List.map (fun (loc : Token.located) -> loc.token) located

let lex_tokens_no_eof src =
  let toks = lex_tokens src in
  List.filter (fun t -> t <> Token.Eof) toks

let lex_located src = Lexer.tokenize src

(* ---- Positive tests ---- *)

let test_keywords () =
  let src = "fn let mut if else match return struct enum impl trait" in
  let expected =
    Token.[ Fn; Let; Mut; If; Else; Match; Return; Struct; Enum; Impl; Trait ]
  in
  Alcotest.(check tokens_testable) "keywords" expected (lex_tokens_no_eof src)

let test_more_keywords () =
  let src = "for while loop break continue in as pub use mod" in
  let expected =
    Token.[ For; While; Loop; Break; Continue; In; As; Pub; Use; Mod ]
  in
  Alcotest.(check tokens_testable)
    "more keywords" expected (lex_tokens_no_eof src)

let test_self_keywords () =
  let src = "self Self" in
  let expected = Token.[ SelfValue; SelfType ] in
  Alcotest.(check tokens_testable)
    "self keywords" expected (lex_tokens_no_eof src)

let test_bool_literals () =
  let src = "true false" in
  let expected = Token.[ True; False ] in
  Alcotest.(check tokens_testable)
    "bool literals" expected (lex_tokens_no_eof src)

let test_type_keywords () =
  let src = "i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 bool str" in
  let expected =
    Token.[ I8; I16; I32; I64; U8; U16; U32; U64; F32; F64; Bool; Str ]
  in
  Alcotest.(check tokens_testable)
    "type keywords" expected (lex_tokens_no_eof src)

let test_builtin_type_keywords () =
  let src = "String Option Result Vec HashMap" in
  let expected =
    Token.[ StringType; OptionType; ResultType; VecType; HashMapType ]
  in
  Alcotest.(check tokens_testable)
    "builtin type keywords" expected (lex_tokens_no_eof src)

let test_identifiers () =
  let src = "foo bar_baz x123 _private" in
  let expected =
    Token.[ Ident "foo"; Ident "bar_baz"; Ident "x123"; Ident "_private" ]
  in
  Alcotest.(check tokens_testable)
    "identifiers" expected (lex_tokens_no_eof src)

let test_unicode_identifiers () =
  let src =
    "\xd1\x84\xd1\x83\xd0\xbd\xd0\xba\xd1\x86\xd0\xb8\xd1\x8f \
     \xce\xb1\xce\xb2\xce\xb3 \xe5\x90\x8d\xe5\x89\x8d"
  in
  let toks = lex_tokens_no_eof src in
  Alcotest.(check int) "3 unicode idents" 3 (List.length toks);
  match toks with
  | [ Token.Ident s1; Token.Ident s2; Token.Ident s3 ] ->
      Alcotest.(check string)
        "russian" "\xd1\x84\xd1\x83\xd0\xbd\xd0\xba\xd1\x86\xd0\xb8\xd1\x8f" s1;
      Alcotest.(check string) "greek" "\xce\xb1\xce\xb2\xce\xb3" s2;
      Alcotest.(check string) "japanese" "\xe5\x90\x8d\xe5\x89\x8d" s3
  | _ -> Alcotest.fail "expected 3 Ident tokens"

let test_underscore () =
  let src = "_" in
  let expected = Token.[ Underscore ] in
  Alcotest.(check tokens_testable) "underscore" expected (lex_tokens_no_eof src)

let test_integer_literals () =
  let src = "42 0 1_000_000 0xFF 0b101" in
  let expected =
    Token.
      [
        IntLit "42";
        IntLit "0";
        IntLit "1_000_000";
        IntLit "0xFF";
        IntLit "0b101";
      ]
  in
  Alcotest.(check tokens_testable)
    "int literals" expected (lex_tokens_no_eof src)

let test_float_literals () =
  let src = "3.14 1e10 2.5e3 1.0E-5" in
  let expected =
    Token.
      [ FloatLit "3.14"; FloatLit "1e10"; FloatLit "2.5e3"; FloatLit "1.0E-5" ]
  in
  Alcotest.(check tokens_testable)
    "float literals" expected (lex_tokens_no_eof src)

let test_string_literal () =
  let src = {|"hello world"|} in
  let expected = Token.[ StringLit "hello world" ] in
  Alcotest.(check tokens_testable)
    "string literal" expected (lex_tokens_no_eof src)

let test_string_escapes () =
  let src = {|"line\nbreak\ttab\\slash\""|} in
  let expected = Token.[ StringLit "line\nbreak\ttab\\slash\"" ] in
  Alcotest.(check tokens_testable)
    "string escapes" expected (lex_tokens_no_eof src)

let test_operators () =
  let src = "+ - * / % == != < > <= >= && || !" in
  let expected =
    Token.
      [
        Plus;
        Minus;
        Star;
        Slash;
        Percent;
        EqEq;
        NotEq;
        Lt;
        Gt;
        LtEq;
        GtEq;
        AmpAmp;
        PipePipe;
        Bang;
      ]
  in
  Alcotest.(check tokens_testable) "operators" expected (lex_tokens_no_eof src)

let test_assignment_operators () =
  let src = "= += -= *= /=" in
  let expected = Token.[ Eq; PlusEq; MinusEq; StarEq; SlashEq ] in
  Alcotest.(check tokens_testable)
    "assignment operators" expected (lex_tokens_no_eof src)

let test_punctuation () =
  let src = "-> => :: . , ; : ? & ( ) { } [ ]" in
  let expected =
    Token.
      [
        Arrow;
        FatArrow;
        ColonColon;
        Dot;
        Comma;
        Semi;
        Colon;
        Question;
        Amp;
        LParen;
        RParen;
        LBrace;
        RBrace;
        LBracket;
        RBracket;
      ]
  in
  Alcotest.(check tokens_testable)
    "punctuation" expected (lex_tokens_no_eof src)

let test_line_comment () =
  let src = "fn // this is a comment\nlet" in
  let expected = Token.[ Fn; Let ] in
  Alcotest.(check tokens_testable)
    "line comment" expected (lex_tokens_no_eof src)

let test_block_comment () =
  let src = "fn /* block comment */ let" in
  let expected = Token.[ Fn; Let ] in
  Alcotest.(check tokens_testable)
    "block comment" expected (lex_tokens_no_eof src)

let test_nested_block_comment () =
  let src = "fn /* outer /* inner */ still comment */ let" in
  let expected = Token.[ Fn; Let ] in
  Alcotest.(check tokens_testable)
    "nested block comment" expected (lex_tokens_no_eof src)

let test_deeply_nested_block_comment () =
  let src = "fn /* a /* b /* c */ b */ a */ let" in
  let expected = Token.[ Fn; Let ] in
  Alcotest.(check tokens_testable)
    "deeply nested block comment" expected (lex_tokens_no_eof src)

let test_fn_decl () =
  let src = "pub fn add(a: i64, b: i64) -> i64 { a + b }" in
  let expected =
    Token.
      [
        Pub;
        Fn;
        Ident "add";
        LParen;
        Ident "a";
        Colon;
        I64;
        Comma;
        Ident "b";
        Colon;
        I64;
        RParen;
        Arrow;
        I64;
        LBrace;
        Ident "a";
        Plus;
        Ident "b";
        RBrace;
      ]
  in
  Alcotest.(check tokens_testable)
    "fn declaration" expected (lex_tokens_no_eof src)

let test_struct_decl () =
  let src = "struct Point { pub x: f64, y: f64 }" in
  let expected =
    Token.
      [
        Struct;
        Ident "Point";
        LBrace;
        Pub;
        Ident "x";
        Colon;
        F64;
        Comma;
        Ident "y";
        Colon;
        F64;
        RBrace;
      ]
  in
  Alcotest.(check tokens_testable)
    "struct decl" expected (lex_tokens_no_eof src)

let test_match_expr () =
  let src = "match x { Some(v) => v, None => 0 }" in
  let expected =
    Token.
      [
        Match;
        Ident "x";
        LBrace;
        Ident "Some";
        LParen;
        Ident "v";
        RParen;
        FatArrow;
        Ident "v";
        Comma;
        Ident "None";
        FatArrow;
        IntLit "0";
        RBrace;
      ]
  in
  Alcotest.(check tokens_testable) "match expr" expected (lex_tokens_no_eof src)

let test_question_mark () =
  let src = "parse_int(s)?" in
  let expected =
    Token.[ Ident "parse_int"; LParen; Ident "s"; RParen; Question ]
  in
  Alcotest.(check tokens_testable)
    "question mark" expected (lex_tokens_no_eof src)

let test_enum_path () =
  let src = "Shape::Circle(r)" in
  let expected =
    Token.
      [ Ident "Shape"; ColonColon; Ident "Circle"; LParen; Ident "r"; RParen ]
  in
  Alcotest.(check tokens_testable) "enum path" expected (lex_tokens_no_eof src)

let test_generic_syntax () =
  let src = "Vec<i64>" in
  let expected = Token.[ VecType; Lt; I64; Gt ] in
  Alcotest.(check tokens_testable)
    "generic syntax" expected (lex_tokens_no_eof src)

let test_option_result_syntax () =
  let src = "Option<i64> Result<i64, str>" in
  let expected =
    Token.[ OptionType; Lt; I64; Gt; ResultType; Lt; I64; Comma; Str; Gt ]
  in
  Alcotest.(check tokens_testable)
    "Option/Result syntax" expected (lex_tokens_no_eof src)

(* ---- Position tracking tests ---- *)

let test_positions_simple () =
  let toks = lex_located "fn main" in
  match toks with
  | [
   { token = Fn; span = fn_span };
   { token = Ident "main"; span = main_span };
   { token = Eof; _ };
  ] ->
      Alcotest.(check int) "fn line" 1 fn_span.start.line;
      Alcotest.(check int) "fn col" 1 fn_span.start.col;
      Alcotest.(check int) "fn offset" 0 fn_span.start.offset;
      Alcotest.(check int) "main line" 1 main_span.start.line;
      Alcotest.(check int) "main col" 4 main_span.start.col;
      Alcotest.(check int) "main offset" 3 main_span.start.offset
  | _ -> Alcotest.fail "unexpected tokens"

let test_positions_multiline () =
  let src = "fn\nmain" in
  let toks = lex_located src in
  match toks with
  | [
   { token = Fn; span = fn_span };
   { token = Ident "main"; span = main_span };
   { token = Eof; _ };
  ] ->
      Alcotest.(check int) "fn line" 1 fn_span.start.line;
      Alcotest.(check int) "fn col" 1 fn_span.start.col;
      Alcotest.(check int) "main line" 2 main_span.start.line;
      Alcotest.(check int) "main col" 1 main_span.start.col
  | _ -> Alcotest.fail "unexpected tokens"

let test_positions_after_comment () =
  let src = "fn // comment\nmain" in
  let toks = lex_located src in
  match toks with
  | [
   { token = Fn; span = fn_span };
   { token = Ident "main"; span = main_span };
   { token = Eof; _ };
  ] ->
      Alcotest.(check int) "fn line" 1 fn_span.start.line;
      Alcotest.(check int) "main line" 2 main_span.start.line;
      Alcotest.(check int) "main col" 1 main_span.start.col
  | _ -> Alcotest.fail "unexpected tokens"

let test_positions_after_block_comment () =
  let src = "fn /* comment\nspanning lines */ main" in
  let toks = lex_located src in
  match toks with
  | [
   { token = Fn; _ };
   { token = Ident "main"; span = main_span };
   { token = Eof; _ };
  ] ->
      Alcotest.(check int) "main line" 2 main_span.start.line;
      Alcotest.(check int) "main col" 19 main_span.start.col
  | _ -> Alcotest.fail "unexpected tokens"

let test_span_end () =
  let toks = lex_located "fn" in
  match toks with
  | [ { token = Fn; span }; { token = Eof; _ } ] ->
      Alcotest.(check int) "start col" 1 span.start.col;
      Alcotest.(check int) "end col" 3 span.stop.col;
      Alcotest.(check int) "start offset" 0 span.start.offset;
      Alcotest.(check int) "end offset" 2 span.stop.offset
  | _ -> Alcotest.fail "unexpected tokens"

(* ---- Negative tests ---- *)

let test_unexpected_char () =
  match Lexer.tokenize "fn @ let" with
  | _ -> Alcotest.fail "expected Lexer_error"
  | exception Lexer.Lexer_error err ->
      Alcotest.(check bool)
        "error mentions unexpected" true
        (String.length err.msg > 0);
      Alcotest.(check int) "error line" 1 err.pos.line;
      Alcotest.(check int) "error col" 4 err.pos.col

let test_unterminated_string () =
  match Lexer.tokenize {|"hello|} with
  | _ -> Alcotest.fail "expected Lexer_error"
  | exception Lexer.Lexer_error err ->
      Alcotest.(check bool)
        "error mentions unterminated" true
        (string_starts_with ~prefix:"unterminated" err.msg);
      Alcotest.(check int) "error line" 1 err.pos.line

let test_unterminated_block_comment () =
  match Lexer.tokenize "fn /* never closed" with
  | _ -> Alcotest.fail "expected Lexer_error"
  | exception Lexer.Lexer_error err ->
      Alcotest.(check bool)
        "error mentions unterminated" true
        (string_starts_with ~prefix:"unterminated" err.msg);
      Alcotest.(check int) "error line" 1 err.pos.line

let test_error_position_multiline () =
  match Lexer.tokenize "fn\nlet\n@" with
  | _ -> Alcotest.fail "expected Lexer_error"
  | exception Lexer.Lexer_error err ->
      Alcotest.(check int) "error line" 3 err.pos.line;
      Alcotest.(check int) "error col" 1 err.pos.col

(* ---- Empty input ---- *)

let test_empty_input () =
  let toks = lex_tokens "" in
  Alcotest.(check tokens_testable) "empty input" [ Token.Eof ] toks

let test_whitespace_only () =
  let toks = lex_tokens "   \n\t\n  " in
  Alcotest.(check tokens_testable) "whitespace only" [ Token.Eof ] toks

(* ---- Test runner ---- *)

let () =
  Alcotest.run "lexer"
    [
      ( "keywords",
        [
          Alcotest.test_case "basic keywords" `Quick test_keywords;
          Alcotest.test_case "more keywords" `Quick test_more_keywords;
          Alcotest.test_case "self keywords" `Quick test_self_keywords;
          Alcotest.test_case "bool literals" `Quick test_bool_literals;
          Alcotest.test_case "type keywords" `Quick test_type_keywords;
          Alcotest.test_case "builtin types" `Quick test_builtin_type_keywords;
        ] );
      ( "identifiers",
        [
          Alcotest.test_case "basic identifiers" `Quick test_identifiers;
          Alcotest.test_case "unicode identifiers" `Quick
            test_unicode_identifiers;
          Alcotest.test_case "underscore" `Quick test_underscore;
        ] );
      ( "literals",
        [
          Alcotest.test_case "integer literals" `Quick test_integer_literals;
          Alcotest.test_case "float literals" `Quick test_float_literals;
          Alcotest.test_case "string literal" `Quick test_string_literal;
          Alcotest.test_case "string escapes" `Quick test_string_escapes;
        ] );
      ( "operators",
        [
          Alcotest.test_case "basic operators" `Quick test_operators;
          Alcotest.test_case "assignment operators" `Quick
            test_assignment_operators;
          Alcotest.test_case "punctuation" `Quick test_punctuation;
        ] );
      ( "comments",
        [
          Alcotest.test_case "line comment" `Quick test_line_comment;
          Alcotest.test_case "block comment" `Quick test_block_comment;
          Alcotest.test_case "nested block comment" `Quick
            test_nested_block_comment;
          Alcotest.test_case "deeply nested block comment" `Quick
            test_deeply_nested_block_comment;
        ] );
      ( "compound",
        [
          Alcotest.test_case "fn declaration" `Quick test_fn_decl;
          Alcotest.test_case "struct declaration" `Quick test_struct_decl;
          Alcotest.test_case "match expression" `Quick test_match_expr;
          Alcotest.test_case "question mark" `Quick test_question_mark;
          Alcotest.test_case "enum path" `Quick test_enum_path;
          Alcotest.test_case "generic syntax" `Quick test_generic_syntax;
          Alcotest.test_case "option/result syntax" `Quick
            test_option_result_syntax;
        ] );
      ( "positions",
        [
          Alcotest.test_case "simple positions" `Quick test_positions_simple;
          Alcotest.test_case "multiline positions" `Quick
            test_positions_multiline;
          Alcotest.test_case "positions after comment" `Quick
            test_positions_after_comment;
          Alcotest.test_case "positions after block comment" `Quick
            test_positions_after_block_comment;
          Alcotest.test_case "span end" `Quick test_span_end;
        ] );
      ( "errors",
        [
          Alcotest.test_case "unexpected char" `Quick test_unexpected_char;
          Alcotest.test_case "unterminated string" `Quick
            test_unterminated_string;
          Alcotest.test_case "unterminated block comment" `Quick
            test_unterminated_block_comment;
          Alcotest.test_case "error position multiline" `Quick
            test_error_position_multiline;
        ] );
      ( "edge_cases",
        [
          Alcotest.test_case "empty input" `Quick test_empty_input;
          Alcotest.test_case "whitespace only" `Quick test_whitespace_only;
        ] );
    ]
