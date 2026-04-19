open Rgo

(* Helper: parse and resolve, return Ok () or Error msg *)
let resolve_string ?(filename = "<test>") src =
  let ast = Parse_driver.parse_string ~filename src in
  Resolver.resolve ast

let pass_resolve src () =
  match resolve_string src with
  | Ok _resolved -> ()
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

let fail_resolve ~expect src () =
  match resolve_string src with
  | Ok _ -> Alcotest.fail "expected error, but resolution succeeded"
  | Error msg ->
      if not (contains_substring msg expect) then
        Alcotest.failf "error message %S does not contain %S" msg expect

(* ---- Positive fixtures ---- *)

let valid_fn_call =
  {|
fn greet() -> i32 {
    42
}
fn main() {
    let x = greet();
}
|}

let valid_struct_and_field =
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

let valid_enum_variant =
  {|
enum Color {
    Red,
    Green,
    Blue,
}
fn main() {
    let c = Color::Red;
}
|}

let valid_local_binding = {|
fn main() {
    let x = 1;
    let y = x;
}
|}

let valid_fn_params =
  {|
fn add(a: i32, b: i32) -> i32 {
    a + b
}
fn main() {
    let r = add(1, 2);
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
    let r = area(s);
}
|}

let valid_enum_struct_variant =
  {|
enum Msg {
    Quit,
    Move { x: f64, y: f64 },
}
fn handle(m: Msg) {
    match m {
        Msg::Quit => {},
        Msg::Move { x, y } => {},
    }
}
fn main() {
    handle(Msg::Quit);
}
|}

let valid_type_annotations =
  {|
struct Pair {
    first: i32,
    second: i32,
}
fn make() -> Pair {
    Pair { first: 1, second: 2 }
}
fn main() {
    let p = make();
}
|}

let valid_impl_block =
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
}
fn main() {
    let c = Counter::new();
    let v = c.get();
}
|}

let valid_nested_scopes =
  {|
fn main() {
    let x = 1;
    {
        let y = x;
        let z = y;
    }
}
|}

let valid_trait_impl =
  {|
trait Greet {
    fn greet(&self) -> str;
}
struct Person { name: str }
impl Greet for Person {
    fn greet(&self) -> str {
        "hello"
    }
}
fn main() {
    let p = Person { name: "Alice" };
}
|}

let valid_generic_trait_bound =
  {|
trait Display {
    fn show(&self) -> str;
}
fn print_it<T: Display>(val: T) -> i32 {
    42
}
|}

let valid_for_loop_binding =
  {|
fn main() {
    let xs = [1, 2, 3];
    for x in xs {
        let v = x;
    }
}
|}

(* ---- Negative fixtures ---- *)

let undef_var = {|
fn main() {
    let x = y;
}
|}

let undef_fn = {|
fn main() {
    let r = foo();
}
|}

let undef_type = {|
fn make() -> Bogus {
    42
}
|}

let undef_type_in_let = {|
fn main() {
    let x: Bogus = 42;
}
|}

let undef_enum_path = {|
fn main() {
    let v = Bogus::Variant;
}
|}

let undef_variant =
  {|
enum Color {
    Red,
    Green,
}
fn main() {
    let c = Color::Blue;
}
|}

let dup_fn = {|
fn foo() {}
fn foo() {}
|}

let dup_struct = {|
struct Foo { x: i32 }
struct Foo { y: i32 }
|}

let dup_enum = {|
enum Color { Red }
enum Color { Blue }
|}

let undef_trait_in_impl =
  {|
struct Foo { x: i32 }
impl Display for Foo {
    fn show(&self) -> str {
        "foo"
    }
}
|}

let undef_trait_in_generic_bound =
  {|
fn print_it<T: Display>(val: T) -> i32 {
    42
}
|}

let undef_struct_in_constructor =
  {|
fn main() {
    let v = Unknown { x: 1 };
}
|}

let undef_field_in_constructor =
  {|
struct Point { x: f64, y: f64 }
fn main() {
    let p = Point { x: 1.0, z: 2.0 };
}
|}

let positive_tests =
  [
    ("valid function call", `Quick, pass_resolve valid_fn_call);
    ( "valid struct and field access",
      `Quick,
      pass_resolve valid_struct_and_field );
    ("valid enum variant path", `Quick, pass_resolve valid_enum_variant);
    ("valid local binding", `Quick, pass_resolve valid_local_binding);
    ("valid function params", `Quick, pass_resolve valid_fn_params);
    ("valid enum tuple variant", `Quick, pass_resolve valid_enum_tuple_variant);
    ("valid enum struct variant", `Quick, pass_resolve valid_enum_struct_variant);
    ("valid type annotations", `Quick, pass_resolve valid_type_annotations);
    ("valid impl block", `Quick, pass_resolve valid_impl_block);
    ("valid nested scopes", `Quick, pass_resolve valid_nested_scopes);
    ("valid for loop binding", `Quick, pass_resolve valid_for_loop_binding);
    ("valid trait impl", `Quick, pass_resolve valid_trait_impl);
    ("valid generic trait bound", `Quick, pass_resolve valid_generic_trait_bound);
  ]

let negative_tests =
  [
    ("undefined variable", `Quick, fail_resolve ~expect:"undefined" undef_var);
    ("undefined function", `Quick, fail_resolve ~expect:"undefined" undef_fn);
    ( "undefined return type",
      `Quick,
      fail_resolve ~expect:"undefined" undef_type );
    ( "undefined type in let",
      `Quick,
      fail_resolve ~expect:"undefined" undef_type_in_let );
    ( "undefined enum in path",
      `Quick,
      fail_resolve ~expect:"undefined" undef_enum_path );
    ( "undefined variant in enum",
      `Quick,
      fail_resolve ~expect:"undefined" undef_variant );
    ("duplicate function", `Quick, fail_resolve ~expect:"duplicate" dup_fn);
    ("duplicate struct", `Quick, fail_resolve ~expect:"duplicate" dup_struct);
    ("duplicate enum", `Quick, fail_resolve ~expect:"duplicate" dup_enum);
    ( "undefined trait in impl",
      `Quick,
      fail_resolve ~expect:"undefined trait 'Display'" undef_trait_in_impl );
    ( "undefined trait in generic bound",
      `Quick,
      fail_resolve ~expect:"undefined trait 'Display'"
        undef_trait_in_generic_bound );
    ( "undefined struct in constructor",
      `Quick,
      fail_resolve ~expect:"undefined" undef_struct_in_constructor );
    ( "undefined field in constructor",
      `Quick,
      fail_resolve ~expect:"undefined" undef_field_in_constructor );
  ]

let () =
  Alcotest.run "resolver"
    [ ("positive", positive_tests); ("negative", negative_tests) ]
