let parse s = Rgo.Parse_driver.parse_string s

(* --- positive tests: parse succeeds --- *)

let test_hello () =
  let _prog = parse "fn main() { println(\"Hello, world!\"); }" in
  ()

let test_struct () =
  let _prog =
    parse
      "pub struct Point { pub x: f64, pub y: f64 }\n\
       fn dist(a: Point, b: Point) -> f64 { 0.0 }"
  in
  ()

let test_enum () =
  let _prog =
    parse
      "enum Shape {\n\
      \    Circle(f64),\n\
      \    Rectangle { width: f64, height: f64 },\n\
      \    Empty,\n\
       }"
  in
  ()

let test_match () =
  let _prog =
    parse
      "fn area(s: Shape) -> f64 {\n\
      \    match s {\n\
      \        Shape::Circle(r) => 3.14 * r * r,\n\
      \        Shape::Rectangle { width, height } => width * height,\n\
      \        Shape::Empty => 0.0,\n\
      \    }\n\
       }"
  in
  ()

let test_impl () =
  let _prog =
    parse
      "struct Counter { value: i64 }\n\
       impl Counter {\n\
      \    pub fn new() -> Self { Counter { value: 0 } }\n\
      \    pub fn get(&self) -> i64 { self.value }\n\
      \    pub fn inc(&mut self) { self.value = self.value + 1; }\n\
       }"
  in
  ()

let test_trait () =
  let _prog =
    parse
      "trait Display {\n\
      \    fn display(&self) -> String;\n\
       }\n\
       trait Summary {\n\
      \    fn summary(&self) -> String;\n\
      \    fn short(&self) -> String {\n\
      \        self.summary()\n\
      \    }\n\
       }"
  in
  ()

let test_trait_impl () =
  let _prog =
    parse
      "struct Article { pub title: String, pub body: String }\n\
       impl Display for Article {\n\
      \    fn display(&self) -> String {\n\
      \        self.title + \": \" + self.body\n\
      \    }\n\
       }"
  in
  ()

let test_generics () =
  let _prog =
    parse
      "struct Box<T> { value: T }\n\
       impl<T> Box<T> {\n\
      \    pub fn new(v: T) -> Self { Box { value: v } }\n\
      \    pub fn get(&self) -> T { self.value }\n\
       }\n\
       fn first<T>(v: Vec<T>) -> Option<T> {\n\
      \    if v.len() == 0 { return None; }\n\
      \    Some(v[0])\n\
       }"
  in
  ()

let test_generic_bounds () =
  let _prog =
    parse
      "fn print_all<T: Display + Summary>(items: Vec<T>) {\n\
      \    for item in items {\n\
      \        println(item.summary());\n\
      \    }\n\
       }"
  in
  ()

let test_result_question () =
  let _prog =
    parse
      "fn double(s: str) -> Result<i64, str> {\n\
      \    let n = parse_int(s)?;\n\
      \    Ok(n * 2)\n\
       }"
  in
  ()

let test_option () =
  let _prog =
    parse
      "fn first(v: Vec<i64>) -> Option<i64> {\n\
      \    if v.len() == 0 { return None; }\n\
      \    Some(v[0])\n\
       }"
  in
  ()

let test_loops () =
  let _prog =
    parse
      "fn main() {\n\
      \    let mut sum: i64 = 0;\n\
      \    for x in xs { sum = sum + x; }\n\
      \    while i < 10 { i = i + 1; }\n\
      \    loop { break; }\n\
       }"
  in
  ()

let test_if_else () =
  let _prog =
    parse "fn max(a: i64, b: i64) -> i64 {\n    if a > b { a } else { b }\n}"
  in
  ()

let test_if_else_chain () =
  let _prog =
    parse
      "fn classify(x: i64) -> str {\n\
      \    if x > 0 { \"positive\" }\n\
      \    else if x < 0 { \"negative\" }\n\
      \    else { \"zero\" }\n\
       }"
  in
  ()

let test_array_literals () =
  let _prog =
    parse
      "fn main() {\n\
      \    let xs = [1, 2, 3];\n\
      \    let ys: Vec<i64> = [];\n\
      \    let zs = [0; 100];\n\
       }"
  in
  ()

let test_assign_ops () =
  let _prog =
    parse
      "fn main() {\n\
      \    let mut x: i64 = 0;\n\
      \    x += 1;\n\
      \    x -= 2;\n\
      \    x *= 3;\n\
      \    x /= 4;\n\
       }"
  in
  ()

let test_method_chain () =
  let _prog =
    parse
      "fn main() {\n\
      \    let mut v: Vec<i64> = Vec::new();\n\
      \    v.push(1);\n\
      \    v.push(2);\n\
      \    let n = v.len();\n\
       }"
  in
  ()

let test_cast () =
  let _prog = parse "fn main() { let x = 42 as f64; }" in
  ()

let test_ref_types () =
  let _prog =
    parse
      "fn foo(a: &Point) -> &i64 { a }\nfn bar(b: &mut Point) { b.x = 1.0; }"
  in
  ()

let test_tuple_type () =
  let _prog = parse "fn foo(p: (i64, String)) { }" in
  ()

let test_nested_generics () =
  let _prog =
    parse
      "fn foo() -> Option<Vec<i64>> { None }\n\
       fn bar() -> Result<Option<i64>, str> { Ok(None) }"
  in
  ()

let test_let_wildcard () =
  let _prog = parse "fn main() { let _ = foo(); }" in
  ()

let test_let_wildcard_typed () =
  let _prog = parse "fn main() { let _: i64 = 42; }" in
  ()

let test_block_expr () =
  let _prog = parse "fn main() {\n    let x = { let a = 1; a + 2 };\n}" in
  ()

let test_enum_impl () =
  let _prog =
    parse
      "enum Shape { Circle(f64), Square(f64) }\n\
       impl Shape {\n\
      \    pub fn area(&self) -> f64 {\n\
      \        match self {\n\
      \            Shape::Circle(r) => 3.14 * r * r,\n\
      \            Shape::Square(s) => s * s,\n\
      \        }\n\
      \    }\n\
       }"
  in
  ()

let test_struct_literal () =
  let _prog = parse "fn main() {\n    let p = Point { x: 1.0, y: 2.0 };\n}" in
  ()

let test_struct_literal_shorthand () =
  let _prog = parse "fn main() {\n    let p = Point { x, y };\n}" in
  ()

(* --- negative: syntax errors report line references --- *)

let test_error_missing_brace () =
  match parse "fn main() {\n  let x = 1;\n" with
  | exception Rgo.Parse_driver.Parse_error e ->
      Alcotest.(check int) "error line" 3 e.line;
      Alcotest.(check string) "error msg" "syntax error" e.msg
  | _ -> Alcotest.fail "expected parse error"

let test_error_missing_semi () =
  match parse "fn main() { let x = 1 }" with
  | exception Rgo.Parse_driver.Parse_error e ->
      Alcotest.(check int) "error line" 1 e.line;
      Alcotest.(check string) "error msg" "syntax error" e.msg
  | _ -> Alcotest.fail "expected parse error"

let test_error_bad_token () =
  match parse "struct Foo { @@ }" with
  | exception Rgo.Lexer.Lexer_error e ->
      Alcotest.(check int) "error line" 1 e.pos.line;
      Alcotest.(check bool) "has message" true (String.length e.msg > 0)
  | _ -> Alcotest.fail "expected lexer error"

let test_error_missing_fn_body () =
  match parse "fn foo()" with
  | exception Rgo.Parse_driver.Parse_error e ->
      Alcotest.(check int) "error line" 1 e.line;
      Alcotest.(check string) "error msg" "syntax error" e.msg
  | _ -> Alcotest.fail "expected parse error"

let test_error_multiline () =
  match parse "fn main() {\n  let x: i64 = 1;\n  let y =\n}" with
  | exception Rgo.Parse_driver.Parse_error e ->
      Alcotest.(check bool) "error has line" true (e.line >= 3)
  | _ -> Alcotest.fail "expected parse error"

(* --- parse example corpus --- *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let test_example_corpus () =
  let examples_dir = "." in
  let files = Sys.readdir examples_dir in
  Array.sort String.compare files;
  Array.iter
    (fun f ->
      if Filename.check_suffix f ".rg" then (
        let path = Filename.concat examples_dir f in
        let source = read_file path in
        (try
           let _prog = parse source in
           ()
         with
        | Rgo.Parse_driver.Parse_error e ->
            Alcotest.failf "Parse error in %s at line %d, col %d: %s" f e.line
              e.col e.msg
        | Rgo.Lexer.Lexer_error e ->
            Alcotest.failf "Lexer error in %s at line %d, col %d: %s" f
              e.pos.line e.pos.col e.msg);
        ()))
    files

let positive_tests =
  Alcotest.
    [
      test_case "hello" `Quick test_hello;
      test_case "struct" `Quick test_struct;
      test_case "enum" `Quick test_enum;
      test_case "match" `Quick test_match;
      test_case "impl" `Quick test_impl;
      test_case "trait" `Quick test_trait;
      test_case "trait impl" `Quick test_trait_impl;
      test_case "generics" `Quick test_generics;
      test_case "generic bounds" `Quick test_generic_bounds;
      test_case "result question" `Quick test_result_question;
      test_case "option" `Quick test_option;
      test_case "loops" `Quick test_loops;
      test_case "if-else" `Quick test_if_else;
      test_case "if-else chain" `Quick test_if_else_chain;
      test_case "array literals" `Quick test_array_literals;
      test_case "assign ops" `Quick test_assign_ops;
      test_case "method chain" `Quick test_method_chain;
      test_case "cast" `Quick test_cast;
      test_case "ref types" `Quick test_ref_types;
      test_case "tuple type" `Quick test_tuple_type;
      test_case "nested generics" `Quick test_nested_generics;
      test_case "let wildcard" `Quick test_let_wildcard;
      test_case "let wildcard typed" `Quick test_let_wildcard_typed;
      test_case "block expr" `Quick test_block_expr;
      test_case "enum impl" `Quick test_enum_impl;
      test_case "struct literal" `Quick test_struct_literal;
      test_case "struct literal shorthand" `Quick test_struct_literal_shorthand;
    ]

let negative_tests =
  Alcotest.
    [
      test_case "missing brace" `Quick test_error_missing_brace;
      test_case "missing semi" `Quick test_error_missing_semi;
      test_case "bad token" `Quick test_error_bad_token;
      test_case "missing fn body" `Quick test_error_missing_fn_body;
      test_case "multiline error" `Quick test_error_multiline;
    ]

let corpus_tests =
  Alcotest.[ test_case "example corpus" `Quick test_example_corpus ]

let () =
  Alcotest.run "parser"
    [
      ("positive", positive_tests);
      ("negative", negative_tests);
      ("corpus", corpus_tests);
    ]
