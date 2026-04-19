open Rgo

(* Helper: parse, resolve, typecheck, and exhaust-check.
   Returns Ok () or Error msg. *)
let exhaust_string ?(filename = "<test>") src =
  let ast = Parse_driver.parse_string ~filename src in
  match Resolver.resolve ast with
  | Error msg -> Error ("resolve: " ^ msg)
  | Ok resolved -> (
      match Typecheck.typecheck resolved with
      | Error msg -> Error ("typecheck: " ^ msg)
      | Ok typed -> (
          match Exhaust.check typed with
          | Ok _ -> Ok ()
          | Error msg -> Error msg))

let pass src () =
  match exhaust_string src with
  | Ok () -> ()
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
  match exhaust_string src with
  | Ok () -> Alcotest.fail "expected error, but exhaustiveness check succeeded"
  | Error msg ->
      if not (contains_substring msg expect) then
        Alcotest.failf "error message %S does not contain %S" msg expect

(* ======== Positive fixtures ======== *)

(* VAL-SEM-011: Exhaustive matches are accepted *)

let exhaustive_all_variants =
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
    let s = describe(Color::Red);
}
|}

let exhaustive_wildcard =
  {|
enum Direction {
    North,
    South,
    East,
    West,
}
fn is_north(d: Direction) -> bool {
    match d {
        Direction::North => true,
        _ => false,
    }
}
fn main() {
    let b = is_north(Direction::North);
}
|}

let exhaustive_binding_catchall =
  {|
enum Shape {
    Circle,
    Square,
    Triangle,
}
fn name(s: Shape) -> str {
    match s {
        Shape::Circle => "circle",
        other => "not circle",
    }
}
fn main() {
    let n = name(Shape::Circle);
}
|}

let exhaustive_tuple_variants =
  {|
enum Expr {
    Num(i32),
    Add(i32, i32),
}
fn eval(e: Expr) -> i32 {
    match e {
        Expr::Num(n) => n,
        Expr::Add(a, b) => a + b,
    }
}
fn main() {
    let r = eval(Expr::Num(42));
}
|}

let exhaustive_struct_variants =
  {|
enum Shape {
    Point,
    Circle { radius: i64 },
}
fn area(s: Shape) -> i64 {
    match s {
        Shape::Point => 0,
        Shape::Circle { radius } => radius * radius,
    }
}
fn main() {
    let a = area(Shape::Point);
}
|}

let exhaustive_single_variant =
  {|
enum Wrapper {
    Value(i32),
}
fn unwrap(w: Wrapper) -> i32 {
    match w {
        Wrapper::Value(v) => v,
    }
}
fn main() {
    let x = unwrap(Wrapper::Value(1));
}
|}

let exhaustive_nested_match =
  {|
enum AB {
    A,
    B,
}
fn nested(x: AB, y: AB) -> str {
    match x {
        AB::A => match y {
            AB::A => "AA",
            AB::B => "AB",
        },
        AB::B => "B-something",
    }
}
fn main() {
    let s = nested(AB::A, AB::B);
}
|}

(* ======== Negative fixtures ======== *)

(* VAL-SEM-012: Non-exhaustive matches are rejected *)

let missing_one_variant =
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
    }
}
fn main() {
    let s = describe(Color::Red);
}
|}

let missing_multiple_variants =
  {|
enum Direction {
    North,
    South,
    East,
    West,
}
fn name(d: Direction) -> str {
    match d {
        Direction::North => "north",
    }
}
fn main() {
    let s = name(Direction::North);
}
|}

let missing_variant_with_tuple =
  {|
enum Expr {
    Num(i32),
    Add(i32, i32),
    Neg(i32),
}
fn eval(e: Expr) -> i32 {
    match e {
        Expr::Num(n) => n,
        Expr::Add(a, b) => a + b,
    }
}
fn main() {
    let r = eval(Expr::Num(1));
}
|}

let missing_variant_with_struct =
  {|
enum Shape {
    Point,
    Circle { radius: i64 },
    Rect { w: i64, h: i64 },
}
fn area(s: Shape) -> i64 {
    match s {
        Shape::Point => 0,
        Shape::Circle { radius } => radius,
    }
}
fn main() {
    let a = area(Shape::Point);
}
|}

let nested_non_exhaustive =
  {|
enum AB {
    A,
    B,
}
fn nested(x: AB, y: AB) -> str {
    match x {
        AB::A => match y {
            AB::A => "AA",
        },
        AB::B => "B-something",
    }
}
fn main() {
    let s = nested(AB::A, AB::B);
}
|}

(* ======== Test registration ======== *)

let positive_tests =
  [
    ("all variants covered", `Quick, pass exhaustive_all_variants);
    ("wildcard covers all", `Quick, pass exhaustive_wildcard);
    ("binding catchall", `Quick, pass exhaustive_binding_catchall);
    ("tuple variants covered", `Quick, pass exhaustive_tuple_variants);
    ("struct variants covered", `Quick, pass exhaustive_struct_variants);
    ("single variant covered", `Quick, pass exhaustive_single_variant);
    ("nested match exhaustive", `Quick, pass exhaustive_nested_match);
  ]

let negative_tests =
  [
    ( "missing one variant",
      `Quick,
      fail ~expect:"Color::Blue" missing_one_variant );
    ( "missing multiple variants",
      `Quick,
      fail ~expect:"non-exhaustive match" missing_multiple_variants );
    ( "missing variant with tuple fields",
      `Quick,
      fail ~expect:"Expr::Neg" missing_variant_with_tuple );
    ( "missing variant with struct fields",
      `Quick,
      fail ~expect:"Shape::Rect" missing_variant_with_struct );
    ( "nested non-exhaustive inner",
      `Quick,
      fail ~expect:"AB::B" nested_non_exhaustive );
  ]

let () =
  Alcotest.run "exhaust"
    [ ("positive", positive_tests); ("negative", negative_tests) ]
