(* Phase 7 codegen tests: snapshot + e2e Go validation *)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      really_input_string ic n)

let write_file path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let run_cmd cmd =
  let stdout_file = Filename.temp_file "rgo_cg_" ".stdout" in
  let stderr_file = Filename.temp_file "rgo_cg_" ".stderr" in
  let full_cmd =
    Printf.sprintf "%s >%s 2>%s" cmd
      (Filename.quote stdout_file)
      (Filename.quote stderr_file)
  in
  let code = Sys.command full_cmd in
  let stdout = read_file stdout_file in
  let stderr = read_file stderr_file in
  Sys.remove stdout_file;
  Sys.remove stderr_file;
  (code, stdout, stderr)

let contains haystack needle =
  let nl = String.length needle in
  let hl = String.length haystack in
  if nl > hl then false
  else
    let found = ref false in
    for i = 0 to hl - nl do
      if String.sub haystack i nl = needle then found := true
    done;
    !found

(* Helper: compile rgo source to Go, check gofmt-clean, go build, go run *)
let compile_and_check ?expected_output source =
  match Rgo.Driver.compile_string ~filename:"test.rg" source with
  | Error e ->
      let msg =
        match e with
        | Rgo.Driver.Lex_error { msg; _ } -> msg
        | Rgo.Driver.Parse_error { msg; _ } -> msg
        | Rgo.Driver.Resolve_error { msg; _ } -> msg
        | Rgo.Driver.Typecheck_error { msg; _ } -> msg
        | Rgo.Driver.Exhaust_error { msg; _ } -> msg
        | Rgo.Driver.Codegen_error msg -> msg
      in
      Alcotest.fail ("compilation failed: " ^ msg)
  | Ok go_src ->
      (* Check gofmt-clean *)
      let out = Filename.temp_file "rgo_cg_" ".go" in
      write_file out go_src;
      let code, diff, _ =
        run_cmd (Printf.sprintf "gofmt -d %s" (Filename.quote out))
      in
      Alcotest.(check int) "gofmt exit 0" 0 code;
      if diff <> "" then
        Alcotest.fail ("gofmt diff:\n" ^ diff ^ "\nGenerated Go:\n" ^ go_src);
      (* Check go build *)
      let code, _, stderr =
        run_cmd (Printf.sprintf "go build -o /dev/null %s" (Filename.quote out))
      in
      if code <> 0 then
        Alcotest.fail
          ("go build failed:\n" ^ stderr ^ "\nGenerated Go:\n" ^ go_src);
      (* Check go run if expected output provided *)
      (match expected_output with
      | Some expected ->
          let code, stdout, stderr =
            run_cmd (Printf.sprintf "go run %s" (Filename.quote out))
          in
          if code <> 0 then
            Alcotest.fail
              ("go run failed:\n" ^ stderr ^ "\nGenerated Go:\n" ^ go_src);
          Alcotest.(check string) "runtime output" expected stdout
      | None -> ());
      Sys.remove out;
      go_src

let compile_snapshot source =
  match Rgo.Driver.compile_string ~filename:"test.rg" source with
  | Error e ->
      let msg =
        match e with
        | Rgo.Driver.Lex_error { msg; _ } -> msg
        | Rgo.Driver.Parse_error { msg; _ } -> msg
        | Rgo.Driver.Resolve_error { msg; _ } -> msg
        | Rgo.Driver.Typecheck_error { msg; _ } -> msg
        | Rgo.Driver.Exhaust_error { msg; _ } -> msg
        | Rgo.Driver.Codegen_error msg -> msg
      in
      Alcotest.fail ("compilation failed: " ^ msg)
  | Ok go_src -> go_src

(* ---------- Struct tests ---------- *)

let test_struct_basic () =
  let src =
    {|
struct Point {
    pub x: f64,
    pub y: f64,
}

fn main() {
    let p = Point { x: 1.0, y: 2.0 };
    println(p.x);
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool)
    "contains type Point struct" true
    (contains go "type Point struct");
  Alcotest.(check bool) "contains X float64" true (contains go "X float64");
  Alcotest.(check bool) "contains Y float64" true (contains go "Y float64")

let test_struct_pub_private () =
  let src =
    {|
struct User {
    pub name: str,
    age: i32,
}

fn main() {
    let u = User { name: "alice", age: 30 };
    println(u.name);
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool) "exported Name" true (contains go "Name string");
  Alcotest.(check bool)
    "unexported age" true
    (contains go "age" && contains go "int32")

(* ---------- Enum tests ---------- *)

let test_enum_basic () =
  let src =
    {|
enum Color {
    Red,
    Green,
    Blue,
}

fn main() {
    let c = Color::Red;
    println("ok");
}
|}
  in
  let go = compile_and_check ~expected_output:"ok\n" src in
  Alcotest.(check bool)
    "sealed interface" true
    (contains go "type Color interface");
  Alcotest.(check bool) "isColor method" true (contains go "isColor()");
  Alcotest.(check bool)
    "ColorRed type" true
    (contains go "type ColorRed struct{}")

let test_enum_with_fields () =
  let src =
    {|
enum Shape {
    Circle(f64),
    Rectangle { width: f64, height: f64 },
    Empty,
}

fn area(s: Shape) -> f64 {
    match s {
        Shape::Circle(r) => 3.14 * r * r,
        Shape::Rectangle { width, height } => width * height,
        Shape::Empty => 0.0,
    }
}

fn main() {
    let c = Shape::Circle(5.0);
    println(area(c));
}
|}
  in
  let go = compile_and_check ~expected_output:"78.5\n" src in
  Alcotest.(check bool)
    "ShapeCircle with Field0" true
    (contains go "Field0 float64");
  Alcotest.(check bool)
    "ShapeRectangle with Width" true
    (contains go "Width  float64")

(* ---------- Match tests ---------- *)

let test_match_expression () =
  let src =
    {|
enum Dir {
    Up,
    Down,
}

fn name(d: Dir) -> str {
    match d {
        Dir::Up => "up",
        Dir::Down => "down",
    }
}

fn main() {
    println(name(Dir::Up));
}
|}
  in
  let _go = compile_and_check ~expected_output:"up\n" src in
  ()

let test_match_statement () =
  let src =
    {|
enum Color {
    Red,
    Green,
    Blue,
}

fn main() {
    let c = Color::Green;
    match c {
        Color::Red => println("r"),
        Color::Green => println("g"),
        Color::Blue => println("b"),
    };
}
|}
  in
  let _go = compile_and_check ~expected_output:"g\n" src in
  ()

(* ---------- Option tests ---------- *)

let test_option_non_nullable () =
  let src =
    {|
fn find_positive(x: i64) -> Option<i64> {
    if x > 0 {
        return Some(x);
    }
    None
}

fn main() {
    let r = find_positive(42);
    println("done");
}
|}
  in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool) "returns *int64" true (contains go "*int64")

let test_option_nullable () =
  let src =
    {|
enum Status {
    Active,
    Inactive,
}

fn maybe_status(flag: bool) -> Option<Status> {
    if flag {
        return Some(Status::Active);
    }
    None
}

fn main() {
    let s = maybe_status(true);
    println("done");
}
|}
  in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool)
    "uses Option[Status]" true
    (contains go "Option[Status]")

(* ---------- Result tests ---------- *)

let test_result_basic () =
  let src =
    {|
fn divide(a: i64, b: i64) -> Result<i64, str> {
    if b == 0 {
        return Err("division by zero");
    }
    Ok(a / b)
}

fn main() {
    let r = divide(10, 2);
    println("done");
}
|}
  in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool)
    "returns (int64, error)" true
    (contains go "(int64, error)")

(* ---------- Question mark tests ---------- *)

let test_question_mark_result () =
  let src =
    {|
fn inner(x: i64) -> Result<i64, str> {
    if x < 0 {
        return Err("negative");
    }
    Ok(x * 2)
}

fn outer(x: i64) -> Result<i64, str> {
    let n = inner(x)?;
    Ok(n + 1)
}

fn main() {
    let r = outer(5);
    println("done");
}
|}
  in
  let _go = compile_and_check ~expected_output:"done\n" src in
  ()

let test_question_mark_option () =
  let src =
    {|
fn find(x: i64) -> Option<i64> {
    if x > 0 {
        return Some(x);
    }
    None
}

fn double_find(x: i64) -> Option<i64> {
    let n = find(x)?;
    Some(n * 2)
}

fn main() {
    let r = double_find(5);
    println("done");
}
|}
  in
  let _go = compile_and_check ~expected_output:"done\n" src in
  ()

(* ---------- Array literal tests ---------- *)

let test_array_literal () =
  let src = {|
fn main() {
    let xs = [1, 2, 3];
    println("done");
}
|} in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool)
    "contains []int64{1, 2, 3}" true
    (contains go "[]int64{1, 2, 3}")

let test_array_empty_with_annotation () =
  let src =
    {|
fn main() {
    let xs: Vec<i64> = [];
    println("done");
}
|}
  in
  let _go = compile_and_check ~expected_output:"done\n" src in
  ()

let test_array_repeat_small () =
  let src = {|
fn main() {
    let xs = [0; 5];
    println("done");
}
|} in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool)
    "inline repeat" true
    (contains go "[]int64{0, 0, 0, 0, 0}")

let test_array_repeat_large () =
  let src = {|
fn main() {
    let xs = [0; 100];
    println("done");
}
|} in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool) "uses rgo_repeat" true (contains go "rgo_repeat")

(* ---------- Keyword escaping tests ---------- *)

let test_keyword_escaping () =
  let src = {|
fn main() {
    let range: i64 = 42;
    println("done");
}
|} in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool) "range escaped" true (contains go "range_")

(* ---------- Prelude tests ---------- *)

let test_prelude_option_struct () =
  let src =
    {|
enum E { A, B }

fn maybe() -> Option<E> {
    Some(E::A)
}

fn main() {
    let v = maybe();
    println("done");
}
|}
  in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool)
    "Option struct in prelude" true
    (contains go "type Option[T any] struct");
  Alcotest.(check bool) "rgo_some in prelude" true (contains go "func rgo_some");
  Alcotest.(check bool) "rgo_none in prelude" true (contains go "func rgo_none")

let test_prelude_rgo_repeat () =
  let src = {|
fn main() {
    let xs = [0; 100];
    println("done");
}
|} in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool)
    "rgo_repeat in prelude" true
    (contains go "func rgo_repeat")

(* ---------- Determinism test ---------- *)

let test_deterministic_output () =
  let src =
    {|
enum Shape {
    Circle(f64),
    Square(f64),
}

fn area(s: Shape) -> f64 {
    match s {
        Shape::Circle(r) => 3.14 * r * r,
        Shape::Square(side) => side * side,
    }
}

fn main() {
    let c = Shape::Circle(5.0);
    println(area(c));
}
|}
  in
  let go1 = compile_snapshot src in
  let go2 = compile_snapshot src in
  Alcotest.(check string) "deterministic" go1 go2

(* ---------- Control flow tests ---------- *)

let test_if_expression_return () =
  let src =
    {|
fn classify(x: i64) -> str {
    if x > 0 {
        "positive"
    } else {
        "non-positive"
    }
}

fn main() {
    println(classify(5));
}
|}
  in
  let _go = compile_and_check ~expected_output:"positive\n" src in
  ()

let test_for_loop () =
  let src =
    {|
fn main() {
    for x in [10, 20, 30] {
        println(x);
    }
}
|}
  in
  let _go = compile_and_check ~expected_output:"10\n20\n30\n" src in
  ()

let test_while_loop () =
  let src =
    {|
fn main() {
    let mut i: i64 = 0;
    while i < 3 {
        i = i + 1;
    }
    println(i);
}
|}
  in
  let _go = compile_and_check ~expected_output:"3\n" src in
  ()

(* ---------- Non-addressable pointer receiver test ---------- *)

let test_pointer_receiver_non_addressable () =
  let src =
    {|
struct Point {
    pub x: f64,
    pub y: f64,
}

impl Point {
    pub fn get_x(&self) -> f64 {
        self.x
    }
}

fn main() {
    let p = Point { x: 1.0, y: 2.0 };
    println(p.get_x());
}
|}
  in
  let go = compile_and_check src in
  (* Verify it generates pointer receiver *)
  Alcotest.(check bool) "pointer receiver" true (contains go "(self *Point)")

(* ---------- Hello world sanity ---------- *)

let test_hello_world () =
  let src = {|
fn main() {
    println("Hello, world!");
}
|} in
  let _go = compile_and_check ~expected_output:"Hello, world!\n" src in
  ()

let test_println_int () =
  let src = {|
fn main() {
    println(42);
}
|} in
  let _go = compile_and_check ~expected_output:"42\n" src in
  ()

let () =
  Alcotest.run "codegen"
    [
      ( "struct",
        [
          Alcotest.test_case "basic struct" `Quick test_struct_basic;
          Alcotest.test_case "pub/private fields" `Quick test_struct_pub_private;
        ] );
      ( "enum",
        [
          Alcotest.test_case "basic enum" `Quick test_enum_basic;
          Alcotest.test_case "enum with fields" `Quick test_enum_with_fields;
        ] );
      ( "match",
        [
          Alcotest.test_case "match expression" `Quick test_match_expression;
          Alcotest.test_case "match statement" `Quick test_match_statement;
        ] );
      ( "option",
        [
          Alcotest.test_case "non-nullable option" `Quick
            test_option_non_nullable;
          Alcotest.test_case "nullable option" `Quick test_option_nullable;
        ] );
      ("result", [ Alcotest.test_case "basic result" `Quick test_result_basic ]);
      ( "question-mark",
        [
          Alcotest.test_case "? on result" `Quick test_question_mark_result;
          Alcotest.test_case "? on option" `Quick test_question_mark_option;
        ] );
      ( "array",
        [
          Alcotest.test_case "array literal" `Quick test_array_literal;
          Alcotest.test_case "empty array with annotation" `Quick
            test_array_empty_with_annotation;
          Alcotest.test_case "repeat small" `Quick test_array_repeat_small;
          Alcotest.test_case "repeat large" `Quick test_array_repeat_large;
        ] );
      ( "keyword-escaping",
        [ Alcotest.test_case "go keyword escape" `Quick test_keyword_escaping ]
      );
      ( "prelude",
        [
          Alcotest.test_case "Option struct" `Quick test_prelude_option_struct;
          Alcotest.test_case "rgo_repeat" `Quick test_prelude_rgo_repeat;
        ] );
      ( "determinism",
        [
          Alcotest.test_case "deterministic output" `Quick
            test_deterministic_output;
        ] );
      ( "control-flow",
        [
          Alcotest.test_case "if expression return" `Quick
            test_if_expression_return;
          Alcotest.test_case "for loop" `Quick test_for_loop;
          Alcotest.test_case "while loop" `Quick test_while_loop;
        ] );
      ( "pointer-receiver",
        [
          Alcotest.test_case "non-addressable receiver" `Quick
            test_pointer_receiver_non_addressable;
        ] );
      ( "sanity",
        [
          Alcotest.test_case "hello world" `Quick test_hello_world;
          Alcotest.test_case "println int" `Quick test_println_int;
        ] );
    ]
