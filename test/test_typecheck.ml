open Rgo

(* Helper: parse, resolve, and typecheck; return Ok () or Error msg *)
let typecheck_string ?(filename = "<test>") src =
  let ast = Parse_driver.parse_string ~filename src in
  match Resolver.resolve ast with
  | Error msg -> Error ("resolve: " ^ msg)
  | Ok resolved -> Typecheck.typecheck resolved

let pass src () =
  match typecheck_string src with
  | Ok _ -> ()
  | Error msg -> Alcotest.fail (Printf.sprintf "expected success, got: %s" msg)

let contains_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let rec check i =
      if i > hlen - nlen then false
      else if String.sub haystack i nlen = needle then true
      else check (i + 1)
    in
    check 0

let fail ~expect src () =
  match typecheck_string src with
  | Ok _ -> Alcotest.fail "expected error, but typechecking succeeded"
  | Error msg ->
      if not (contains_substring msg expect) then
        Alcotest.failf "error message %S does not contain %S" msg expect

(* ======== Positive fixtures ======== *)

(* VAL-SEM-005: Well-typed programs pass typecheck with local inference *)

let valid_let_inference =
  {|
fn main() {
    let x = 42;
    let y = x;
    let s = "hello";
    let b = true;
    let f = 3.14;
}
|}

let valid_fn_call_types =
  {|
fn add(a: i32, b: i32) -> i32 {
    a + b
}
fn main() {
    let r = add(1, 2);
}
|}

let valid_struct_fields =
  {|
struct Point {
    x: f64,
    y: f64,
}
fn main() {
    let p = Point { x: 1.0, y: 2.0 };
    let a = p.x;
}
|}

let valid_enum_match =
  {|
enum Color {
    Red,
    Green,
    Blue,
}
fn describe(c: Color) -> str {
    match c {
        Color::Red => "red",
        Color::Green => "green",
        Color::Blue => "blue",
    }
}
fn main() {
    let c = Color::Red;
    let s = describe(c);
}
|}

let valid_enum_tuple_variant =
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
    let s = Shape::Circle(5.0);
    let a = area(s);
}
|}

let valid_enum_struct_variant =
  {|
enum Msg {
    Quit,
    Move { x: f64, y: f64 },
}
fn handle(m: Msg) -> f64 {
    match m {
        Msg::Quit => 0.0,
        Msg::Move { x, y } => x + y,
    }
}
fn main() {
    let m = Msg::Quit;
    let r = handle(m);
}
|}

let valid_impl_method_call =
  {|
struct Counter {
    count: i32,
}
impl Counter {
    fn new() -> Counter {
        Counter { count: 0 }
    }
    fn get(&self) -> i32 {
        self.count
    }
    fn inc(&mut self) {
        self.count = self.count + 1;
    }
}
fn main() {
    let mut c = Counter::new();
    c.inc();
    let v = c.get();
}
|}

let valid_if_expr =
  {|
fn max(a: i32, b: i32) -> i32 {
    if a > b {
        a
    } else {
        b
    }
}
fn main() {
    let m = max(3, 5);
}
|}

let valid_for_loop =
  {|
fn main() {
    let xs = [1, 2, 3];
    for x in xs {
        let y = x;
    }
}
|}

let valid_nested_scopes =
  {|
fn main() {
    let x = 1;
    {
        let y = x;
        let z = y + 1;
    }
}
|}

let valid_while_loop =
  {|
fn main() {
    let mut i = 0;
    while i < 10 {
        i = i + 1;
    }
}
|}

let valid_array_literal =
  {|
fn main() {
    let xs = [1, 2, 3];
    let first = xs[0];
}
|}

let valid_array_repeat = {|
fn main() {
    let xs = [0; 10];
}
|}

(* VAL-SEM-008: Result question-mark works *)
let valid_result_question_mark =
  {|
fn bar() -> Result<i32, str> {
    Ok(42)
}
fn foo() -> Result<i32, str> {
    let n = bar()?;
    Ok(n + 1)
}
|}

(* VAL-SEM-009: Option question-mark works *)
let valid_option_question_mark =
  {|
fn bar() -> Option<i32> {
    Some(42)
}
fn foo() -> Option<i32> {
    let n = bar()?;
    Some(n + 1)
}
|}

let valid_self_in_impl =
  {|
struct Point {
    x: f64,
    y: f64,
}
impl Point {
    fn new(x: f64, y: f64) -> Self {
        Point { x: x, y: y }
    }
}
fn main() {
    let p = Point::new(1.0, 2.0);
}
|}

let valid_string_concat =
  {|
fn main() {
    let a = "hello";
    let b = " world";
    let c = a + b;
}
|}

let valid_vec_typed_empty = {|
fn main() {
    let xs: Vec<i32> = [];
}
|}

(* ======== Negative fixtures ======== *)

(* VAL-SEM-006: Mismatched call arguments are rejected *)
let mismatch_call_arg =
  {|
fn add(a: i32, b: i32) -> i32 {
    a + b
}
fn main() {
    let r = add(1, "hello");
}
|}

let wrong_arg_count =
  {|
fn add(a: i32, b: i32) -> i32 {
    a + b
}
fn main() {
    let r = add(1);
}
|}

(* VAL-SEM-007: Incompatible assignments or returns are rejected *)
let mismatch_return = {|
fn foo() -> i32 {
    "hello"
}
|}

let mismatch_let_annotation = {|
fn main() {
    let x: i32 = "hello";
}
|}

let mismatch_assign = {|
fn main() {
    let mut x = 42;
    x = "hello";
}
|}

(* VAL-SEM-010: Invalid question-mark usage is rejected *)
let question_on_non_result = {|
fn foo() -> i32 {
    let x = 42?;
    x
}
|}

let question_result_in_non_result =
  {|
fn bar() -> Result<i32, str> {
    Ok(42)
}
fn foo() -> i32 {
    let n = bar()?;
    n
}
|}

let question_option_in_non_option =
  {|
fn bar() -> Option<i32> {
    Some(42)
}
fn foo() -> i32 {
    let n = bar()?;
    n
}
|}

(* VAL-SEM-013: Self is rejected outside trait and impl contexts *)
let self_outside_impl = {|
fn foo() -> Self {
    42
}
|}

(* VAL-SEM-014: Mutability rules are enforced *)
let mutating_on_immutable =
  {|
fn main() {
    let xs = [1, 2, 3];
    xs.push(4);
}
|}

let assign_to_immutable = {|
fn main() {
    let x = 42;
    x = 43;
}
|}

let assign_field_immutable =
  {|
struct Point { x: f64, y: f64 }
fn main() {
    let p = Point { x: 1.0, y: 2.0 };
    p.x = 3.0;
}
|}

(* VAL-SEM-015: Empty array literals without type context are rejected *)
let empty_array_no_context = {|
fn main() {
    let xs = [];
}
|}

(* --- Regression: incompatible ? propagation error types --- *)
let question_result_error_mismatch =
  {|
fn bar() -> Result<i32, i64> {
    Ok(42)
}
fn foo() -> Result<i32, str> {
    let n = bar()?;
    Ok(n)
}
|}

let question_option_in_result_fn =
  {|
fn bar() -> Option<i32> {
    Some(42)
}
fn foo() -> Result<i32, str> {
    let n = bar()?;
    Ok(n)
}
|}

let question_result_in_option_fn =
  {|
fn bar() -> Result<i32, str> {
    Ok(42)
}
fn foo() -> Option<i32> {
    let n = bar()?;
    Some(n)
}
|}

(* --- Regression: trait impl missing required methods --- *)
let trait_impl_missing_method =
  {|
trait Greet {
    fn hello(&self) -> str;
    fn goodbye(&self) -> str;
}
struct Bob {}
impl Greet for Bob {
    fn hello(&self) -> str {
        "hello"
    }
}
fn main() {}
|}

(* --- Regression: trait impl signature mismatch --- *)
let trait_impl_sig_mismatch =
  {|
trait Greet {
    fn hello(&self, name: str) -> str;
}
struct Bob {}
impl Greet for Bob {
    fn hello(&self) -> str {
        "hello"
    }
}
fn main() {}
|}

let trait_impl_return_mismatch =
  {|
trait Greet {
    fn hello(&self) -> str;
}
struct Bob {}
impl Greet for Bob {
    fn hello(&self) -> i32 {
        42
    }
}
fn main() {}
|}

(* --- Positive: valid trait impl --- *)
let valid_trait_impl =
  {|
trait Greet {
    fn hello(&self) -> str;
}
struct Bob {}
impl Greet for Bob {
    fn hello(&self) -> str {
        "hello"
    }
}
fn main() {}
|}

(* Arithmetic on non-numeric *)
let arithmetic_on_bool = {|
fn main() {
    let x = true + false;
}
|}

(* Index with non-integer *)
let index_non_integer =
  {|
fn main() {
    let xs = [1, 2, 3];
    let v = xs["hello"];
}
|}

(* ======== Test registration ======== *)

let positive_tests =
  [
    ("local inference", `Quick, pass valid_let_inference);
    ("function call types", `Quick, pass valid_fn_call_types);
    ("struct field types", `Quick, pass valid_struct_fields);
    ("enum match types", `Quick, pass valid_enum_match);
    ("enum tuple variant", `Quick, pass valid_enum_tuple_variant);
    ("enum struct variant", `Quick, pass valid_enum_struct_variant);
    ("impl method call", `Quick, pass valid_impl_method_call);
    ("if expression", `Quick, pass valid_if_expr);
    ("for loop", `Quick, pass valid_for_loop);
    ("nested scopes", `Quick, pass valid_nested_scopes);
    ("while loop", `Quick, pass valid_while_loop);
    ("array literal", `Quick, pass valid_array_literal);
    ("array repeat", `Quick, pass valid_array_repeat);
    ("Result ? propagation", `Quick, pass valid_result_question_mark);
    ("Option ? propagation", `Quick, pass valid_option_question_mark);
    ("Self in impl", `Quick, pass valid_self_in_impl);
    ("string concat", `Quick, pass valid_string_concat);
    ("empty Vec with annotation", `Quick, pass valid_vec_typed_empty);
    ("valid trait impl", `Quick, pass valid_trait_impl);
  ]

let negative_tests =
  [
    ( "mismatched call arg type",
      `Quick,
      fail ~expect:"type mismatch" mismatch_call_arg );
    ( "wrong argument count",
      `Quick,
      fail ~expect:"expects 2 argument(s), got 1" wrong_arg_count );
    ( "mismatched return type",
      `Quick,
      fail ~expect:"type mismatch" mismatch_return );
    ( "mismatched let annotation",
      `Quick,
      fail ~expect:"type mismatch" mismatch_let_annotation );
    ( "mismatched assign type",
      `Quick,
      fail ~expect:"type mismatch" mismatch_assign );
    ( "? on non-Result/Option",
      `Quick,
      fail ~expect:"expected Result or Option" question_on_non_result );
    ( "? Result in non-Result fn",
      `Quick,
      fail ~expect:"cannot use `?` on Result" question_result_in_non_result );
    ( "? Option in non-Option fn",
      `Quick,
      fail ~expect:"cannot use `?` on Option" question_option_in_non_option );
    ("Self outside impl", `Quick, fail ~expect:"Self" self_outside_impl);
    ( "mutating on immutable",
      `Quick,
      fail ~expect:"immutable" mutating_on_immutable );
    ("assign to immutable", `Quick, fail ~expect:"immutable" assign_to_immutable);
    ( "assign field of immutable",
      `Quick,
      fail ~expect:"immutable" assign_field_immutable );
    ( "empty array no type context",
      `Quick,
      fail ~expect:"cannot infer element type" empty_array_no_context );
    ("arithmetic on bool", `Quick, fail ~expect:"non-numeric" arithmetic_on_bool);
    ( "index with non-integer",
      `Quick,
      fail ~expect:"index must be integer" index_non_integer );
    ( "? Result error type mismatch",
      `Quick,
      fail ~expect:"error type mismatch" question_result_error_mismatch );
    ( "? Option in Result-returning fn",
      `Quick,
      fail ~expect:"cannot use `?` on Option" question_option_in_result_fn );
    ( "? Result in Option-returning fn",
      `Quick,
      fail ~expect:"cannot use `?` on Result" question_result_in_option_fn );
    ( "trait impl missing required method",
      `Quick,
      fail ~expect:"missing required method" trait_impl_missing_method );
    ( "trait impl param count mismatch",
      `Quick,
      fail ~expect:"signature mismatch" trait_impl_sig_mismatch );
    ( "trait impl return type mismatch",
      `Quick,
      fail ~expect:"signature mismatch" trait_impl_return_mismatch );
  ]

let () =
  Alcotest.run "typecheck"
    [ ("positive", positive_tests); ("negative", negative_tests) ]
