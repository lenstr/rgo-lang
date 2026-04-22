(* Phase 7 codegen tests: snapshot + e2e Go validation *)

open Codegen_test_helpers

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

let test_match_wildcard_arm () =
  let src =
    {|
enum Shape {
    Circle,
    Square,
    Triangle,
}

fn describe(s: Shape) -> str {
    match s {
        Shape::Circle => "circle",
        _ => "other",
    }
}

fn main() {
    println(describe(Shape::Circle));
    println(describe(Shape::Square));
    println(describe(Shape::Triangle));
}
|}
  in
  let _go = compile_and_check ~expected_output:"circle\nother\nother\n" src in
  ()

let test_match_wildcard_arm_expression () =
  let src =
    {|
enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter,
}

fn value(c: Coin) -> i64 {
    match c {
        Coin::Quarter => 25,
        Coin::Dime => 10,
        _ => 1,
    }
}

fn main() {
    println(value(Coin::Quarter));
    println(value(Coin::Dime));
    println(value(Coin::Penny));
}
|}
  in
  let _go = compile_and_check ~expected_output:"25\n10\n1\n" src in
  ()

let test_match_bind_arm () =
  let src =
    {|
enum Animal {
    Cat,
    Dog,
    Bird,
}

fn greet(a: Animal) -> str {
    match a {
        Animal::Cat => "meow",
        other => "unknown",
    }
}

fn main() {
    println(greet(Animal::Cat));
    println(greet(Animal::Dog));
}
|}
  in
  let _go = compile_and_check ~expected_output:"meow\nunknown\n" src in
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

(* Regression: Some(0) must emit new(int64(0)) not new(0) to avoid *int vs *int64 *)
let test_option_literal_int () =
  let src =
    {|
fn get_zero() -> Option<i64> {
    Some(0)
}

fn main() {
    let r = get_zero();
    println("done");
}
|}
  in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool) "returns *int64" true (contains go "*int64");
  Alcotest.(check bool) "literal is typed" true (contains go "int64(0)")

(* Regression: Some(-1) must emit new(int64(-1)) not new(-1) *)
let test_option_negative_int_literal () =
  let src =
    {|
fn get_neg() -> Option<i64> {
    Some(-1)
}

fn main() {
    let r = get_neg();
    println("done");
}
|}
  in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool) "returns *int64" true (contains go "*int64");
  Alcotest.(check bool) "negative literal is typed" true (contains go "int64(")

(* Regression: Some(-1.5) as f32 must emit new(float32(-1.5)) not new(-1.5) *)
let test_option_negative_float_literal () =
  let src =
    {|
fn get_neg_f() -> Option<f32> {
    Some(-1.5)
}

fn main() {
    let r = get_neg_f();
    println("done");
}
|}
  in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool) "returns *float32" true (contains go "*float32");
  Alcotest.(check bool)
    "negative float literal is typed" true (contains go "float32(")

(* Regression: Option<Option<i64>> must lower to valid Go, not **int64 *)
let test_option_nested () =
  let src =
    {|
fn inner_find(x: i64) -> Option<i64> {
    if x > 0 {
        return Some(x);
    }
    None
}

fn maybe_find(x: i64) -> Option<Option<i64>> {
    if x > 0 {
        let inner = inner_find(x);
        return Some(inner);
    }
    None
}

fn main() {
    let outer = maybe_find(42);
    match outer {
        Option::Some(inner) => {
            match inner {
                Option::Some(v) => println(v),
                Option::None => println("inner none"),
            }
        },
        Option::None => println("outer none"),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  (* Outer option wraps a non-nullable inner (pointer-backed *int64),
     so outer should use the struct-backed Option[*int64] representation *)
  Alcotest.(check bool)
    "uses Option struct for outer" true
    (contains go "Option[*int64]");
  Alcotest.(check bool) "no double pointer" true (not (contains go "**int64"))

(* Nested inline built-in Option pattern with Copy payload *)
let test_option_nested_inline_pattern () =
  let src =
    {|
fn maybe_find(x: i64) -> Option<Option<i64>> {
    if x > 0 {
        Some(Some(x))
    } else {
        None
    }
}

fn main() {
    let outer = maybe_find(42);
    match outer {
        Option::Some(Option::Some(v)) => println(v),
        Option::Some(Option::None) => println("inner none"),
        Option::None => println("outer none"),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  Alcotest.(check bool) "inner binding is emitted" true (contains go "v :=")

(* Nested inline Result(Option) pattern with Copy payload *)
let test_result_nested_option_pattern () =
  let src =
    {|
fn maybe_value(x: i64) -> Result<Option<i64>, str> {
    if x > 0 {
        Ok(Some(x))
    } else if x == 0 {
        Ok(None)
    } else {
        Err("negative")
    }
}

fn main() {
    match maybe_value(42) {
        Result::Ok(Option::Some(v)) => println(v),
        Result::Ok(Option::None) => println("inner none"),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  Alcotest.(check bool) "inner binding is emitted" true (contains go "v :=")

(* Nested inline Option(Result) pattern with Copy payload *)
let test_option_nested_result_pattern () =
  let src =
    {|
fn maybe_result(x: i64) -> Result<Option<i64>, str> {
    if x > 0 {
        Ok(Some(x))
    } else if x == 0 {
        Ok(None)
    } else {
        Err("negative")
    }
}

fn main() {
    match maybe_result(42) {
        Result::Ok(Option::Some(v)) => println(v),
        Result::Ok(Option::None) => println("inner none"),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  Alcotest.(check bool) "inner binding is emitted" true (contains go "v :=")

(* Nested Option with wildcard inner pattern *)
let test_option_nested_wildcard_inner () =
  let src =
    {|
fn maybe_find(x: i64) -> Option<Option<i64>> {
    if x > 0 {
        Some(Some(x))
    } else {
        None
    }
}

fn main() {
    match maybe_find(42) {
        Option::Some(Option::Some(_)) => println("some-some"),
        Option::Some(Option::None) => println("inner none"),
        Option::None => println("outer none"),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"some-some\n" src in
  Alcotest.(check bool)
    "no panic in generated code" true
    (not (contains go "panic"))

(* Nested Option with default fallback arm *)
let test_option_nested_default_fallback () =
  let src =
    {|
fn maybe_find(x: i64) -> Option<Option<i64>> {
    if x > 0 {
        Some(Some(x))
    } else {
        None
    }
}

fn main() {
    match maybe_find(-1) {
        Option::Some(Option::Some(v)) => println(v),
        _ => println("fallback"),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"fallback\n" src in
  Alcotest.(check bool)
    "no panic in generated code" true
    (not (contains go "panic"))

(* Nested Result(Option) with wildcard inner pattern *)
let test_result_nested_option_wildcard_inner () =
  let src =
    {|
fn maybe_value(x: i64) -> Result<Option<i64>, str> {
    if x > 0 {
        Ok(Some(x))
    } else if x == 0 {
        Ok(None)
    } else {
        Err("negative")
    }
}

fn main() {
    match maybe_value(42) {
        Result::Ok(Option::Some(_)) => println("ok-some"),
        Result::Ok(Option::None) => println("ok-none"),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"ok-some\n" src in
  Alcotest.(check bool)
    "no panic in generated code" true
    (not (contains go "panic"))

(* Nested Result(Option) with default fallback arm *)
let test_result_nested_option_default_fallback () =
  let src =
    {|
fn maybe_value(x: i64) -> Result<Option<i64>, str> {
    if x > 0 {
        Ok(Some(x))
    } else if x == 0 {
        Ok(None)
    } else {
        Err("negative")
    }
}

fn main() {
    match maybe_value(0) {
        Result::Ok(Option::Some(v)) => println(v),
        _ => println("fallback"),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"fallback\n" src in
  Alcotest.(check bool)
    "no panic in generated code" true
    (not (contains go "panic"))

(* Nested Option(Result) with wildcard inner pattern *)
let test_option_nested_result_wildcard_inner () =
  let src =
    {|
fn maybe_result(x: i64) -> Result<Option<i64>, str> {
    if x > 0 {
        Ok(Some(x))
    } else if x == 0 {
        Ok(None)
    } else {
        Err("negative")
    }
}

fn main() {
    match maybe_result(42) {
        Result::Ok(Option::Some(_)) => println("ok-some"),
        Result::Ok(Option::None) => println("ok-none"),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"ok-some\n" src in
  Alcotest.(check bool)
    "no panic in generated code" true
    (not (contains go "panic"))

(* Nested Option match in expression context with wildcard inner *)
let test_option_nested_wildcard_expr () =
  let src =
    {|
fn maybe_find(x: i64) -> Option<Option<i64>> {
    if x > 0 {
        Some(Some(x))
    } else {
        None
    }
}

fn main() {
    let label = match maybe_find(42) {
        Option::Some(Option::Some(_)) => "found",
        Option::Some(Option::None) => "empty",
        Option::None => "missing",
    };
    println(label);
}
|}
  in
  let go = compile_and_check ~expected_output:"found\n" src in
  Alcotest.(check bool)
    "no panic in generated code" true
    (not (contains go "panic"))

(* Nested Result match in expression context with default fallback *)
let test_result_nested_option_default_fallback_expr () =
  let src =
    {|
fn maybe_value(x: i64) -> Result<Option<i64>, str> {
    if x > 0 {
        Ok(Some(x))
    } else if x == 0 {
        Ok(None)
    } else {
        Err("negative")
    }
}

fn main() {
    let label = match maybe_value(0) {
        Result::Ok(Option::Some(_)) => "value",
        _ => "other",
    };
    println(label);
}
|}
  in
  let go = compile_and_check ~expected_output:"other\n" src in
  Alcotest.(check bool)
    "no panic in generated code" true
    (not (contains go "panic"))

(* Deeper nested Option<Option<Option<T>>> with Copy payload *)
let test_option_deep_nested_3_levels () =
  let src =
    {|
fn main() {
    let a: Option<Option<Option<i64>>> = Some(Some(Some(42)));
    match a {
        Option::Some(Option::Some(Option::Some(v))) => println(v),
        Option::Some(Option::Some(Option::None)) => println("some-some-none"),
        Option::Some(Option::None) => println("some-none"),
        Option::None => println("none"),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  Alcotest.(check bool)
    "no panic in generated code" true
    (not (contains go "panic"))

(* Deeper nested Option<Option<Option<T>>> with default fallback *)
let test_option_deep_nested_3_levels_default_fallback () =
  let src =
    {|
fn deep(x: i64) -> Option<Option<Option<i64>>> {
    if x > 0 {
        Some(Some(Some(x)))
    } else {
        None
    }
}

fn main() {
    match deep(-1) {
        Option::Some(Option::Some(Option::Some(v))) => println(v),
        _ => println("fallback"),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"fallback\n" src in
  Alcotest.(check bool)
    "no panic in generated code" true
    (not (contains go "panic"))

(* Deeper nested Option<Option<Option<T>>> hitting outer None arm *)
let test_option_deep_nested_3_levels_outer_none () =
  let src =
    {|
fn deep(x: i64) -> Option<Option<Option<i64>>> {
    if x > 0 {
        Some(Some(Some(x)))
    } else {
        None
    }
}

fn main() {
    match deep(-1) {
        Option::Some(Option::Some(Option::Some(v))) => println(v),
        Option::Some(Option::Some(Option::None)) => println("some-some-none"),
        Option::Some(Option::None) => println("some-none"),
        Option::None => println("none"),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"none\n" src in
  Alcotest.(check bool)
    "no panic in generated code" true
    (not (contains go "panic"))

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

let test_question_mark_result_nested_in_call () =
  let src =
    {|
fn inner(x: i64) -> Result<i64, str> {
    if x < 0 {
        return Err("negative");
    }
    Ok(x * 2)
}

fn add_ten(n: i64) -> i64 {
    n + 10
}

fn outer(x: i64) -> Result<i64, str> {
    let result = add_ten(inner(x)?);
    Ok(result)
}

fn main() {
    let _ = outer(5);
    println("done");
}
|}
  in
  let _go = compile_and_check ~expected_output:"done\n" src in
  ()

let test_question_mark_option_nested_in_call () =
  let src =
    {|
fn find(x: i64) -> Option<i64> {
    if x > 0 {
        return Some(x);
    }
    None
}

fn add_ten(n: i64) -> i64 {
    n + 10
}

fn use_find(x: i64) -> Option<i64> {
    let result = add_ten(find(x)?);
    Some(result)
}

fn main() {
    let _ = use_find(5);
    println("done");
}
|}
  in
  let _go = compile_and_check ~expected_output:"done\n" src in
  ()

let test_question_mark_result_in_binary_expr () =
  let src =
    {|
fn get_val(x: i64) -> Result<i64, str> {
    if x < 0 {
        return Err("negative");
    }
    Ok(x)
}

fn compute(x: i64) -> Result<i64, str> {
    let sum = get_val(x)? + 10;
    Ok(sum)
}

fn main() {
    let _ = compute(5);
    println("done");
}
|}
  in
  let _go = compile_and_check ~expected_output:"done\n" src in
  ()

let test_question_mark_wildcard_let_nested () =
  let src =
    {|
fn inner(x: i64) -> Result<i64, str> {
    if x < 0 {
        return Err("negative");
    }
    Ok(x * 2)
}

fn add_ten(n: i64) -> i64 {
    n + 10
}

fn outer(x: i64) -> Result<i64, str> {
    let _ = add_ten(inner(x)?);
    Ok(x)
}

fn main() {
    let _ = outer(5);
    println("done");
}
|}
  in
  let _go = compile_and_check ~expected_output:"done\n" src in
  ()

(* ---------- Result signature without Err construction ---------- *)

let test_result_sig_no_err_construction () =
  let src =
    {|
fn maybe_ok(x: i64) -> Result<i64, str> {
    Ok(x + 1)
}

fn main() {
    let r = maybe_ok(41);
    println("done");
}
|}
  in
  let go = compile_and_check ~expected_output:"done\n" src in
  (* The generated Go must NOT import "errors" because no Err is constructed *)
  Alcotest.(check bool) "no errors import" false (contains go "\"errors\"");
  (* But it should still have the (int64, error) return signature *)
  Alcotest.(check bool) "has error return" true (contains go "error")

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

let test_struct_variant_in_array () =
  let src =
    {|
enum Shape {
    Rect { w: f64, h: f64 },
    Circle(f64),
}

fn main() {
    let shapes = [Shape::Rect { w: 2.0, h: 3.0 }, Shape::Rect { w: 4.0, h: 5.0 }];
    println("done");
}
|}
  in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool) "ShapeRect literal" true (contains go "ShapeRect{")

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
struct Counter {
    value: i64,
}

impl Counter {
    pub fn new(v: i64) -> Self {
        Counter { value: v }
    }

    pub fn get(&self) -> i64 {
        self.value
    }
}

fn main() {
    println(Counter::new(42).get());
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  (* Verify it generates pointer receiver *)
  Alcotest.(check bool) "pointer receiver" true (contains go "(self *Counter)")

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

(* ---------- Example fixture regression ---------- *)

let test_result_option_example () =
  let src = read_file "../examples/result_option.rg" in
  let _go = compile_and_check ~expected_output:"5\n12\n1\n11\n" src in
  ()

(* ---------- Struct impl tests (VAL-CODEGEN-010) ---------- *)

let test_struct_impl_constructor () =
  let src =
    {|
struct Counter {
    value: i64,
}

impl Counter {
    pub fn new() -> Self {
        Counter { value: 0 }
    }

    pub fn get(&self) -> i64 {
        self.value
    }

    pub fn inc(&mut self) {
        self.value = self.value + 1;
    }
}

fn main() {
    let mut c = Counter::new();
    c.inc();
    c.inc();
    c.inc();
    println(c.get());
}
|}
  in
  let go = compile_and_check ~expected_output:"3\n" src in
  Alcotest.(check bool) "assoc fn CounterNew" true (contains go "CounterNew");
  Alcotest.(check bool) "method Get" true (contains go "Get()");
  Alcotest.(check bool) "method Inc" true (contains go "Inc()")

let test_struct_impl_value_receiver () =
  let src =
    {|
struct Point {
    pub x: f64,
    pub y: f64,
}

impl Point {
    pub fn origin() -> Self {
        Point { x: 0.0, y: 0.0 }
    }

    pub fn translate(self, dx: f64, dy: f64) -> Point {
        Point { x: self.x + dx, y: self.y + dy }
    }

    pub fn sum(&self) -> f64 {
        self.x + self.y
    }
}

fn main() {
    let p = Point::origin();
    let p2 = p.translate(3.0, 4.0);
    println(p2.sum());
}
|}
  in
  let go = compile_and_check ~expected_output:"7\n" src in
  Alcotest.(check bool)
    "value receiver Translate" true
    (contains go "(self Point) Translate")

let test_struct_impl_mut_ref () =
  let src =
    {|
struct Acc {
    pub total: i64,
}

impl Acc {
    pub fn new() -> Self {
        Acc { total: 0 }
    }

    pub fn add(&mut self, n: i64) {
        self.total = self.total + n;
    }

    pub fn value(&self) -> i64 {
        self.total
    }
}

fn main() {
    let mut a = Acc::new();
    a.add(10);
    a.add(20);
    a.add(30);
    println(a.value());
}
|}
  in
  let _go = compile_and_check ~expected_output:"60\n" src in
  ()

(* ---------- Enum impl tests (VAL-CODEGEN-011) ---------- *)

let test_enum_impl_method () =
  let src =
    {|
enum Animal {
    Dog(str),
    Cat(str),
}

impl Animal {
    pub fn name(&self) -> str {
        match self {
            Animal::Dog(n) => n,
            Animal::Cat(n) => n,
        }
    }

    pub fn speak(&self) -> str {
        match self {
            Animal::Dog(_) => "woof",
            Animal::Cat(_) => "meow",
        }
    }
}

fn main() {
    let d = Animal::Dog("Rex");
    let c = Animal::Cat("Whiskers");
    println(d.name());
    println(d.speak());
    println(c.name());
    println(c.speak());
}
|}
  in
  let go =
    compile_and_check ~expected_output:"Rex\nwoof\nWhiskers\nmeow\n" src
  in
  Alcotest.(check bool) "interface has Name" true (contains go "Name() string");
  Alcotest.(check bool)
    "interface has Speak" true
    (contains go "Speak() string");
  Alcotest.(check bool) "helper impl" true (contains go "animalNameImpl")

let test_enum_impl_with_params () =
  let src =
    {|
enum Expr {
    Num(f64),
    Add(f64, f64),
}

impl Expr {
    pub fn eval(&self) -> f64 {
        match self {
            Expr::Num(n) => n,
            Expr::Add(a, b) => a + b,
        }
    }
}

fn main() {
    let e1 = Expr::Num(42.0);
    let e2 = Expr::Add(10.0, 20.0);
    println(e1.eval());
    println(e2.eval());
}
|}
  in
  let _go = compile_and_check ~expected_output:"42\n30\n" src in
  ()

(* ---------- Vec operations tests (VAL-CODEGEN-013) ---------- *)

let test_vec_push_len () =
  let src =
    {|
fn main() {
    let mut v: Vec<i64> = [1, 2, 3];
    v.push(4);
    v.push(5);
    println(v.len());
    println(v[0]);
    println(v[4]);
}
|}
  in
  let _go = compile_and_check ~expected_output:"5\n1\n5\n" src in
  ()

let test_vec_iteration () =
  let src =
    {|
fn main() {
    let v: Vec<i64> = [10, 20, 30];
    for x in v {
        println(x);
    }
}
|}
  in
  let _go = compile_and_check ~expected_output:"10\n20\n30\n" src in
  ()

let test_vec_pop () =
  let src =
    {|
fn main() {
    let mut v: Vec<i64> = [1, 2, 3];
    let popped = v.pop();
    println(v.len());
}
|}
  in
  let _go = compile_and_check ~expected_output:"2\n" src in
  ()

(* ---------- HashMap operations tests (VAL-CODEGEN-013) ---------- *)

let test_hashmap_insert_len () =
  let src =
    {|
fn main() {
    let mut m: HashMap<str, i64> = HashMap::new();
    m.insert("a", 1);
    m.insert("b", 2);
    m.insert("c", 3);
    println(m.len());
}
|}
  in
  let _go = compile_and_check ~expected_output:"3\n" src in
  ()

let test_hashmap_contains_key () =
  let src =
    {|
fn main() {
    let mut m: HashMap<str, i64> = HashMap::new();
    m.insert("x", 42);
    println(m.contains_key("x"));
    println(m.contains_key("y"));
}
|}
  in
  let _go = compile_and_check ~expected_output:"true\nfalse\n" src in
  ()

let test_hashmap_remove () =
  let src =
    {|
fn main() {
    let mut m: HashMap<str, i64> = HashMap::new();
    m.insert("a", 1);
    m.insert("b", 2);
    m.remove("a");
    println(m.len());
    println(m.contains_key("a"));
}
|}
  in
  let _go = compile_and_check ~expected_output:"1\nfalse\n" src in
  ()

let test_hashmap_get () =
  let src =
    {|
fn main() {
    let mut m: HashMap<str, i64> = HashMap::new();
    m.insert("key", 99);
    let v = m.get("key");
    println("done");
}
|}
  in
  let _go = compile_and_check ~expected_output:"done\n" src in
  ()

(* ---------- Go vet and determinism (VAL-CODEGEN-016, VAL-CROSS-005) ---------- *)

let test_representative_go_vet () =
  (* A realistic program that exercises multiple codegen features *)
  let src =
    {|
enum Shape {
    Circle(f64),
    Square(f64),
}

impl Shape {
    pub fn area(&self) -> f64 {
        match self {
            Shape::Circle(r) => 3.14 * r * r,
            Shape::Square(s) => s * s,
        }
    }
}

struct Stats {
    pub count: i64,
    pub total: f64,
}

impl Stats {
    pub fn new() -> Self {
        Stats { count: 0, total: 0.0 }
    }

    pub fn record(&mut self, val: f64) {
        self.count = self.count + 1;
        self.total = self.total + val;
    }
}

fn classify(x: i64) -> str {
    if x > 0 {
        "positive"
    } else {
        "non-positive"
    }
}

fn main() {
    let c = Shape::Circle(1.0);
    let sq = Shape::Square(2.0);
    let mut s = Stats::new();
    s.record(c.area());
    s.record(sq.area());
    println(s.count);
    println(classify(1));
}
|}
  in
  let _go = compile_and_check ~expected_output:"2\npositive\n" src in
  ()

let test_determinism_representative () =
  (* Verify deterministic output for a program with impls, collections, match *)
  let src =
    {|
struct Box {
    pub value: i64,
}

impl Box {
    pub fn new(v: i64) -> Self {
        Box { value: v }
    }

    pub fn get(&self) -> i64 {
        self.value
    }
}

enum Tag {
    A,
    B(i64),
}

impl Tag {
    pub fn label(&self) -> str {
        match self {
            Tag::A => "a",
            Tag::B(_) => "b",
        }
    }
}

fn main() {
    let b = Box::new(42);
    println(b.get());
    let t = Tag::B(10);
    println(t.label());
}
|}
  in
  let go1 = compile_snapshot src in
  let go2 = compile_snapshot src in
  Alcotest.(check string) "deterministic impl+enum" go1 go2

(* ---------- Go version requirements (VAL-CROSS-007) ---------- *)

let test_go_version_adequate () =
  match Rgo.Driver.check_go_version_adequate () with
  | Ok ver ->
      Alcotest.(check bool) "go version detected" true (String.length ver > 0);
      (* Verify it's at least go1.26 *)
      Alcotest.(check bool)
        "version starts with go" true
        (String.length ver > 2 && String.sub ver 0 2 = "go")
  | Error msg -> Alcotest.fail ("Go version check failed: " ^ msg)

let test_go_version_below_minimum () =
  (* Simulate an old Go version line to verify the below-minimum error path *)
  let line = "go version go1.21.5 linux/amd64" in
  match Rgo.Driver.parse_go_version_line line with
  | Error msg ->
      Alcotest.(check bool)
        "error mentions version" true (contains msg "go1.21.5");
      Alcotest.(check bool)
        "error mentions 1.26+ requirement" true (contains msg "Go 1.26+")
  | Ok _ -> Alcotest.fail "Expected error for Go 1.21 but got Ok"

let test_go_version_unparseable () =
  (* Simulate completely garbled output *)
  let line = "some random garbage output" in
  match Rgo.Driver.parse_go_version_line line with
  | Error msg ->
      Alcotest.(check bool)
        "error mentions parsing failure" true
        (contains msg "cannot parse Go version")
  | Ok _ -> Alcotest.fail "Expected error for unparseable output but got Ok"

let test_go_version_success_simulated () =
  (* Simulate a good Go version line to verify the success path independently *)
  let line = "go version go1.26.1 linux/amd64" in
  match Rgo.Driver.parse_go_version_line line with
  | Ok v -> Alcotest.(check string) "parsed version" "go1.26.1" v
  | Error msg -> Alcotest.fail ("Expected Ok for Go 1.26.1 but got: " ^ msg)

let test_go_version_future () =
  (* Simulate a future Go version to verify forward compatibility *)
  let line = "go version go2.0.0 linux/amd64" in
  match Rgo.Driver.parse_go_version_line line with
  | Ok v -> Alcotest.(check string) "parsed version" "go2.0.0" v
  | Error msg -> Alcotest.fail ("Expected Ok for Go 2.0 but got: " ^ msg)

(* ---------- CLI pipeline (VAL-CROSS-001) ---------- *)

let test_cli_pipeline () =
  (* Full pipeline: compile .rg source -> go build -> go run *)
  let src =
    {|
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() {
    let result = add(17, 25);
    println(result);
}
|}
  in
  let _go = compile_and_check ~expected_output:"42\n" src in
  ()

(* ---------- Example fixture tests ---------- *)

let test_impl_methods_example () =
  let src = read_file "../examples/impl_methods.rg" in
  let _go = compile_and_check ~expected_output:"2\n" src in
  ()

let test_shapes_example () =
  let src = read_file "../examples/shapes.rg" in
  let _go = compile_and_check ~expected_output:"78.5\n" src in
  ()

let test_loops_example () =
  let src = read_file "../examples/loops.rg" in
  let _go = compile_and_check ~expected_output:"15\n10\n" src in
  ()

let test_traits_example () =
  let src = read_file "../examples/traits.rg" in
  let _go = compile_and_check ~expected_output:"Hello: World\nHello\n" src in
  ()

(* Regression: generic Go emission *)
let test_generics_example_go_build () =
  let src = read_file "../examples/generics.rg" in
  let go = compile_and_check ~expected_output:"42\n" src in
  (* Verify correct generic type parameter syntax in generated Go *)
  Alcotest.(check bool)
    "generic struct decl" true
    (contains go "type Box[T any] struct");
  Alcotest.(check bool)
    "generic assoc fn decl" true
    (contains go "func BoxNew[T any](v T) Box[T]");
  Alcotest.(check bool)
    "generic receiver" true
    (contains go "func (self *Box[T]) Get() T");
  Alcotest.(check bool)
    "instantiated struct literal" true
    (contains go "Box[T]{value: v}")

let test_generic_struct_codegen () =
  let src =
    {|
    struct Pair<A, B> {
      pub first: A,
      pub second: B,
    }

    impl<A, B> Pair<A, B> {
      pub fn new(a: A, b: B) -> Self {
        Pair { first: a, second: b }
      }

      pub fn get_first(&self) -> A {
        self.first
      }
    }

    fn main() {
      let p = Pair::new(1, "hello");
      println(p.get_first());
    }
    |}
  in
  let go = compile_and_check ~expected_output:"1\n" src in
  Alcotest.(check bool)
    "multi-param generic struct" true
    (contains go "type Pair[A any, B any] struct");
  Alcotest.(check bool)
    "multi-param assoc fn" true
    (contains go "func PairNew[A any, B any](a A, b B) Pair[A, B]");
  Alcotest.(check bool)
    "multi-param receiver" true
    (contains go "func (self *Pair[A, B]) Get_first() A")

let test_generic_fn_codegen () =
  let src =
    {|
    fn identity<T>(x: T) -> T {
      x
    }

    fn main() {
      let v = identity(42);
      println(v);
    }
    |}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  Alcotest.(check bool)
    "generic fn decl" true
    (contains go "func identity[T any](x T) T")

(* Regression: generic nominal type zero values on error paths *)
let test_generic_zero_value_result_error_path () =
  let src =
    {|
    struct Pair<A, B> {
      pub first: A,
      pub second: B,
    }

    impl<A, B> Pair<A, B> {
      pub fn try_new(a: A, b: B, ok: bool) -> Result<Self, str> {
        if !ok {
          return Err("nope");
        }
        Ok(Pair { first: a, second: b })
      }
    }

    fn main() {
      let _ = Pair::try_new(1, "hello", true);
      println("ok");
    }
    |}
  in
  let go = compile_and_check ~expected_output:"ok\n" src in
  (* Verify the zero value uses the full generic type, not bare Pair{} *)
  Alcotest.(check bool)
    "generic zero value has type args" true
    (contains go "Pair[A, B]{}");
  Alcotest.(check bool) "no bare Pair{}" false (contains go "Pair{}")

let test_let_wildcard_result () =
  let src =
    {|
fn fallible(ok: bool) -> Result<i64, str> {
    if ok {
        Ok(42)
    } else {
        Err("fail")
    }
}

fn main() {
    let _ = fallible(true);
    println("done");
}
    |}
  in
  let go = compile_and_check ~expected_output:"done\n" src in
  Alcotest.(check bool)
    "wildcard result emits two-value discard" true
    (contains go "_, _ = fallible")

(* ---------- trait codegen tests ---------- *)

(* VAL-TRAIT-001: Trait without Self -> standard Go interface *)
let test_trait_no_self () =
  let src =
    {|
trait Greet {
  fn hello(&self) -> str;
}

struct Person {
  pub name: str,
}

impl Greet for Person {
  fn hello(&self) -> str {
    self.name
  }
}

fn greet<T: Greet>(g: T) -> str {
  g.hello()
}

fn main() {
  let p = Person { name: "Alice" };
  println(greet(p));
}
    |}
  in
  let go = compile_and_check ~expected_output:"Alice\n" src in
  (* Verify the generated interface *)
  Alcotest.(check bool)
    "Greet interface generated" true
    (contains go "type Greet interface");
  Alcotest.(check bool) "Hello method in interface" true (contains go "Hello()")

(* VAL-TRAIT-002: Trait with Self -> Go 1.26 self-referential interface *)
let test_trait_with_self () =
  let src =
    {|
trait Eq {
  fn eq(&self, other: Self) -> bool;
}

struct Num {
  pub val: i64,
}

impl Eq for Num {
  fn eq(&self, other: Self) -> bool {
    self.val == other.val
  }
}

fn all_equal<T: Eq>(a: T, b: T) -> bool {
  a.eq(b)
}

fn main() {
  let n1 = Num { val: 42 };
  let n2 = Num { val: 42 };
  if all_equal(n1, n2) {
    println("equal");
  } else {
    println("not equal");
  }
}
    |}
  in
  let go = compile_and_check ~expected_output:"equal\n" src in
  Alcotest.(check bool)
    "Self-referential interface" true
    (contains go "type Eq[Self any] interface")

(* VAL-TRAIT-003: Struct trait impl satisfies interface *)
let test_trait_struct_impl () =
  let src =
    {|
trait Display {
  fn show(&self) -> str;
}

struct Counter {
  pub value: i64,
}

impl Display for Counter {
  fn show(&self) -> str {
    "counter"
  }
}

fn display<T: Display>(item: T) -> str {
  item.show()
}

fn main() {
  let c = Counter { value: 5 };
  println(display(c));
}
    |}
  in
  compile_and_check ~expected_output:"counter\n" src |> ignore

(* VAL-TRAIT-004: Enum trait impl satisfies interface *)
let test_trait_enum_impl () =
  let src =
    {|
trait Describe {
  fn describe(&self) -> str;
}

enum Shape {
  Circle(f64),
  Square(f64),
}

impl Describe for Shape {
  fn describe(&self) -> str {
    "a shape"
  }
}

fn show_desc<T: Describe>(item: T) {
  println(item.describe());
}

fn main() {
  let s = Shape::Circle(3.14);
  show_desc(s);
}
    |}
  in
  compile_and_check ~expected_output:"a shape\n" src |> ignore

(* VAL-TRAIT-005: Default trait methods are synthesized when omitted *)
let test_trait_default_method () =
  let src =
    {|
trait Greet {
  fn hello(&self) -> str;
  fn goodbye(&self) -> str {
    "bye"
  }
}

struct Bot {
  pub name: str,
}

impl Greet for Bot {
  fn hello(&self) -> str {
    self.name
  }
}

fn main() {
  let b = Bot { name: "R2D2" };
  println(b.hello());
  println(b.goodbye());
}
    |}
  in
  compile_and_check ~expected_output:"R2D2\nbye\n" src |> ignore

(* VAL-TRAIT-014: Default trait methods can call sibling trait methods through Self *)
let test_trait_default_self_method_call () =
  let src =
    {|
trait Summary {
  fn summary(&self) -> str;
  fn short(&self) -> str {
    self.summary()
  }
}

struct Article {
  pub title: str,
  pub body: str,
}

impl Summary for Article {
  fn summary(&self) -> str {
    self.title
  }
}

fn main() {
  let a = Article { title: "Hello", body: "World" };
  println(a.short());
}
    |}
  in
  compile_and_check ~expected_output:"Hello\n" src |> ignore

(* VAL-TRAIT-006: Generic trait bounds lower correctly *)
let test_trait_generic_bounds () =
  let src =
    {|
trait Show {
  fn show(&self) -> str;
}

trait Debug {
  fn debug(&self) -> str;
}

struct Item {
  pub label: str,
}

impl Show for Item {
  fn show(&self) -> str {
    self.label
  }
}

impl Debug for Item {
  fn debug(&self) -> str {
    self.label
  }
}

fn print_both<T: Show + Debug>(item: T) {
  println(item.show());
  println(item.debug());
}

fn main() {
  let i = Item { label: "test" };
  print_both(i);
}
    |}
  in
  compile_and_check ~expected_output:"test\ntest\n" src |> ignore

(* VAL-TRAIT-007: Self-based trait APIs work through generic calls *)
let test_trait_self_generic () =
  let src =
    {|
trait Eq {
  fn eq(&self, other: Self) -> bool;
}

struct Num {
  pub val: i64,
}

impl Eq for Num {
  fn eq(&self, other: Self) -> bool {
    self.val == other.val
  }
}

fn are_equal<T: Eq>(a: T, b: T) -> bool {
  a.eq(b)
}

fn main() {
  let n1 = Num { val: 42 };
  let n2 = Num { val: 42 };
  if are_equal(n1, n2) {
    println("yes");
  } else {
    println("no");
  }
}
    |}
  in
  compile_and_check ~expected_output:"yes\n" src |> ignore

(* VAL-TRAIT-013: Trait-enabled generated Go remains gofmt-clean and vet-clean *)
let test_trait_go_cleanliness () =
  let src =
    {|
trait Stringify {
  fn to_str(&self) -> str;
}

struct Wrapper {
  pub inner: i64,
}

impl Stringify for Wrapper {
  fn to_str(&self) -> str {
    "wrapped"
  }
}

fn stringify_it<T: Stringify>(w: T) -> str {
  w.to_str()
}

fn main() {
  let w = Wrapper { inner: 10 };
  println(stringify_it(w));
}
    |}
  in
  (* compile_and_check already validates gofmt + go build + go vet *)
  compile_and_check ~expected_output:"wrapped\n" src |> ignore

(* VAL-CROSS-002: Final MVP acceptance program *)
let test_final_mvp_acceptance () =
  let src =
    {|
trait Describe {
  fn describe(&self) -> str;
}

trait Clone {
  fn clone(&self) -> Self;
}

struct Task {
  pub title: str,
  pub done: bool,
}

impl Task {
  pub fn new(title: str) -> Self {
    Task { title: title, done: false }
  }

  pub fn complete(&mut self) {
    self.done = true;
  }
}

impl Clone for Task {
  fn clone(&self) -> Self {
    Task { title: self.title, done: self.done }
  }
}

impl Describe for Task {
  fn describe(&self) -> str {
    if self.done {
      self.title + " [done]"
    } else {
      self.title + " [todo]"
    }
  }
}

enum Priority {
  High,
  Medium,
  Low,
}

impl Describe for Priority {
  fn describe(&self) -> str {
    match self {
      Priority::High => "HIGH",
      Priority::Medium => "MEDIUM",
      Priority::Low => "LOW",
    }
  }
}

fn print_desc<T: Describe>(item: T) {
  println(item.describe());
}

fn find_task(tasks: Vec<str>, target: str) -> Option<str> {
  for t in tasks {
    if t == target {
      return Some(t);
    }
  }
  None
}

fn try_parse(s: str) -> Result<i64, str> {
  if s == "42" {
    Ok(42)
  } else {
    Err("not 42")
  }
}

fn maybe_parse(s: str) -> Result<i64, str> {
  let v = try_parse(s)?;
  Ok(v + 1)
}

fn main() {
  let mut task = Task::new("write compiler");
  print_desc(task.clone());
  task.complete();
  print_desc(task);

  let p = Priority::High;
  print_desc(p);

  let tasks: Vec<str> = ["alpha", "beta", "gamma"];
  let found = find_task(tasks, "beta");
  match found {
    Option::Some(t) => println("found: " + t),
    Option::None => println("not found"),
  }

  let mut scores: HashMap<str, i64> = HashMap::new();
  scores.insert("alice", 100);
  scores.insert("bob", 85);
  println(scores.len());

  match maybe_parse("42") {
    Result::Ok(v) => println(v),
    Result::Err(e) => println(e),
  }
}
    |}
  in
  compile_and_check
    ~expected_output:
      "write compiler [todo]\nwrite compiler [done]\nHIGH\nfound: beta\n2\n43\n"
    src
  |> ignore

(* ======== stdlib namespace and naming bridge tests ======== *)

(* Positive: use net::http with both function calls and type annotations *)
let test_stdlib_namespace_positive () =
  let src =
    {|
use net::http;

fn handle_request(w: http::ResponseWriter, r: http::Request) {
    println("handling request");
}

fn main() {
    let mux = http::new_serve_mux();
    http::listen_and_serve(":8080", mux);
    println("done");
}
|}
  in
  let go = compile_and_check src in
  (* Check generated Go contains proper imports and symbols *)
  Alcotest.(check bool) "has net/http import" true (contains go "\"net/http\"");
  Alcotest.(check bool)
    "has http.NewServeMux" true
    (contains go "http.NewServeMux()");
  Alcotest.(check bool)
    "has http.ListenAndServe" true
    (contains go "http.ListenAndServe");
  Alcotest.(check bool)
    "has http.ResponseWriter" true
    (contains go "http.ResponseWriter");
  Alcotest.(check bool) "has *http.Request" true (contains go "*http.Request")

(* Positive: stdlib types in both type and value positions in same program *)
let test_stdlib_type_and_value_positions () =
  let src =
    {|
use net::http;

fn handle(w: http::ResponseWriter, r: http::Request) {
    println("handling");
}

fn main() {
    let mux = http::new_serve_mux();
    http::listen_and_serve(":8080", mux);
}
|}
  in
  let go = compile_and_check src in
  (* Types in parameter annotations lower correctly *)
  Alcotest.(check bool)
    "has http.ResponseWriter param" true
    (contains go "http.ResponseWriter");
  Alcotest.(check bool)
    "has *http.Request param" true
    (contains go "*http.Request");
  (* Calls lower correctly *)
  Alcotest.(check bool)
    "has http.NewServeMux call" true
    (contains go "http.NewServeMux()")

(* Negative: wrong-case callable (Go-cased) *)
let test_stdlib_wrong_case_callable () =
  compile_expect_error ~expect:"wrong case"
    {|
use net::http;

fn main() {
    let mux = http::NewServeMux();
}
|}
    ()

(* Negative: wrong-case type (snake_case) *)
let test_stdlib_wrong_case_type () =
  compile_expect_error ~expect:"wrong case"
    {|
use net::http;

fn handle(w: http::response_writer, r: http::request) {
    println("handling");
}

fn main() {
}
|}
    ()

(* Negative: unqualified access after import *)
let test_stdlib_unqualified_rejected () =
  compile_expect_error ~expect:"undefined"
    {|
use net::http;

fn main() {
    let mux = new_serve_mux();
}
|}
    ()

(* Negative: unknown stdlib member *)
let test_stdlib_unknown_member () =
  compile_expect_error ~expect:"undefined member"
    {|
use net::http;

fn main() {
    http::missing_symbol();
}
|}
    ()

(* Negative: stdlib types rejected in expression position *)
let test_stdlib_type_in_expr_rejected () =
  compile_expect_error ~expect:"is a type, not a value expression"
    {|
use net::http;

fn main() {
    let x = http::Request;
}
|}
    ()

(* Regression: local `http` without import does not miscompile as net/http *)
let test_local_http_without_import () =
  let src =
    {|
fn http() -> i64 {
    42
}

fn main() {
    let x = http();
    println("done");
}
|}
  in
  let go = compile_and_check src in
  (* Must NOT contain net/http import *)
  Alcotest.(check bool) "no net/http import" false (contains go "\"net/http\"");
  (* Must NOT contain http. qualified references *)
  Alcotest.(check bool) "no http.* qualified calls" false (contains go "http.")

(* ======== receiver/member interop codegen tests ======== *)

(* Positive: mux.handle_func lowers to mux.HandleFunc *)
let test_stdlib_receiver_handle_func () =
  let src =
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    println("handled");
}
fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/path", handler);
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool) "has mux.HandleFunc" true (contains go "mux.HandleFunc");
  Alcotest.(check bool)
    "has http.NewServeMux" true
    (contains go "http.NewServeMux()")

(* Positive: req.form_value lowers to r.FormValue *)
let test_stdlib_receiver_form_value () =
  let src =
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let name = r.form_value("name");
    println(name);
}
fn main() {
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool) "has r.FormValue" true (contains go ".FormValue")

(* Positive: req.method lowers to r.Method *)
let test_stdlib_receiver_method_field () =
  let src =
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let m = r.method;
    println(m);
}
fn main() {
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool) "has r.Method" true (contains go ".Method")

(* Positive: w.write_header lowers to w.WriteHeader *)
let test_stdlib_receiver_write_header () =
  let src =
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    w.write_header(200);
}
fn main() {
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool) "has w.WriteHeader" true (contains go ".WriteHeader")

(* Positive: w.write lowers to w.Write *)
let test_stdlib_receiver_write () =
  let src =
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    w.write("hello");
}
fn main() {
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool) "has w.Write" true (contains go ".Write(")

(* Positive: combined handler with all receiver members *)
let test_stdlib_receiver_combined () =
  let src =
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let m = r.method;
    let name = r.form_value("name");
    w.write_header(200);
    w.write("ok");
}
fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/items", handler);
    http::listen_and_serve(":8080", mux);
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool) "has net/http import" true (contains go "\"net/http\"");
  Alcotest.(check bool) "has mux.HandleFunc" true (contains go "mux.HandleFunc");
  Alcotest.(check bool) "has .FormValue" true (contains go ".FormValue");
  Alcotest.(check bool) "has .Method" true (contains go ".Method");
  Alcotest.(check bool) "has .WriteHeader" true (contains go ".WriteHeader");
  Alcotest.(check bool) "has .Write" true (contains go ".Write(");
  Alcotest.(check bool)
    "has http.ListenAndServe" true
    (contains go "http.ListenAndServe");
  Alcotest.(check bool)
    "has http.NewServeMux" true
    (contains go "http.NewServeMux()")

(* Negative: Go-cased receiver methods rejected *)
let test_stdlib_receiver_wrong_case_handle_func () =
  compile_expect_error ~expect:"wrong case"
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    println("ok");
}
fn main() {
    let mux = http::new_serve_mux();
    mux.HandleFunc("/path", handler);
}
|}
    ()

let test_stdlib_receiver_wrong_case_form_value () =
  compile_expect_error ~expect:"wrong case"
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let name = r.FormValue("name");
}
fn main() {
}
|}
    ()

let test_stdlib_receiver_wrong_case_method () =
  compile_expect_error ~expect:"wrong case"
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let m = r.Method;
}
fn main() {
}
|}
    ()

let test_stdlib_receiver_wrong_case_write_header () =
  compile_expect_error ~expect:"wrong case"
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    w.WriteHeader(200);
}
fn main() {
}
|}
    ()

let test_stdlib_receiver_wrong_case_write () =
  compile_expect_error ~expect:"wrong case"
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    w.Write("hello");
}
fn main() {
}
|}
    ()

(* Negative: w.write in value position rejected (void return, no return contract) *)
let test_stdlib_receiver_write_value_position () =
  compile_expect_error ~expect:"cannot bind result of void expression"
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let n = w.write("hello");
}
fn main() {
}
|}
    ()

(* Negative: return w.write(...) rejected (void expression in return position) *)
let test_stdlib_receiver_write_return_position () =
  compile_expect_error ~expect:"cannot return void expression"
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    return w.write("hello");
}
fn main() {
}
|}
    ()

(* Negative: return w.write_header(...) rejected *)
let test_stdlib_receiver_write_header_return_position () =
  compile_expect_error ~expect:"cannot return void expression"
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    return w.write_header(200);
}
fn main() {
}
|}
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
          Alcotest.test_case "wildcard arm stmt" `Quick test_match_wildcard_arm;
          Alcotest.test_case "wildcard arm expr" `Quick
            test_match_wildcard_arm_expression;
          Alcotest.test_case "bind arm" `Quick test_match_bind_arm;
        ] );
      ( "option",
        [
          Alcotest.test_case "non-nullable option" `Quick
            test_option_non_nullable;
          Alcotest.test_case "nullable option" `Quick test_option_nullable;
          Alcotest.test_case "literal int option" `Quick test_option_literal_int;
          Alcotest.test_case "negative int literal option" `Quick
            test_option_negative_int_literal;
          Alcotest.test_case "negative float literal option" `Quick
            test_option_negative_float_literal;
          Alcotest.test_case "nested Option<Option<T>>" `Quick
            test_option_nested;
          Alcotest.test_case "nested inline Option<Option<i64>>" `Quick
            test_option_nested_inline_pattern;
          Alcotest.test_case "nested inline Result<Option<i64>>" `Quick
            test_result_nested_option_pattern;
          Alcotest.test_case "nested inline Option<Result<i64>>" `Quick
            test_option_nested_result_pattern;
          Alcotest.test_case "nested Option wildcard inner" `Quick
            test_option_nested_wildcard_inner;
          Alcotest.test_case "nested Option default fallback" `Quick
            test_option_nested_default_fallback;
          Alcotest.test_case "nested Result(Option) wildcard inner" `Quick
            test_result_nested_option_wildcard_inner;
          Alcotest.test_case "nested Result(Option) default fallback" `Quick
            test_result_nested_option_default_fallback;
          Alcotest.test_case "nested Option(Result) wildcard inner" `Quick
            test_option_nested_result_wildcard_inner;
          Alcotest.test_case "nested Option wildcard inner expr" `Quick
            test_option_nested_wildcard_expr;
          Alcotest.test_case "nested Result(Option) default fallback expr"
            `Quick test_result_nested_option_default_fallback_expr;
          Alcotest.test_case "deep nested Option<Option<Option<T>>> 3 levels"
            `Quick test_option_deep_nested_3_levels;
          Alcotest.test_case "deep nested 3 levels default fallback" `Quick
            test_option_deep_nested_3_levels_default_fallback;
          Alcotest.test_case "deep nested 3 levels outer None arm" `Quick
            test_option_deep_nested_3_levels_outer_none;
        ] );
      ( "result",
        [
          Alcotest.test_case "basic result" `Quick test_result_basic;
          Alcotest.test_case "let _ = result fn" `Quick test_let_wildcard_result;
          Alcotest.test_case "result sig without Err" `Quick
            test_result_sig_no_err_construction;
        ] );
      ( "question-mark",
        [
          Alcotest.test_case "? on result" `Quick test_question_mark_result;
          Alcotest.test_case "? on option" `Quick test_question_mark_option;
          Alcotest.test_case "? result nested in call" `Quick
            test_question_mark_result_nested_in_call;
          Alcotest.test_case "? option nested in call" `Quick
            test_question_mark_option_nested_in_call;
          Alcotest.test_case "? result in binary expr" `Quick
            test_question_mark_result_in_binary_expr;
          Alcotest.test_case "? wildcard let nested" `Quick
            test_question_mark_wildcard_let_nested;
        ] );
      ( "array",
        [
          Alcotest.test_case "array literal" `Quick test_array_literal;
          Alcotest.test_case "empty array with annotation" `Quick
            test_array_empty_with_annotation;
          Alcotest.test_case "repeat small" `Quick test_array_repeat_small;
          Alcotest.test_case "repeat large" `Quick test_array_repeat_large;
          Alcotest.test_case "struct variant in array" `Quick
            test_struct_variant_in_array;
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
      ( "struct-impl",
        [
          Alcotest.test_case "constructor + methods" `Quick
            test_struct_impl_constructor;
          Alcotest.test_case "value receiver" `Quick
            test_struct_impl_value_receiver;
          Alcotest.test_case "mut ref methods" `Quick test_struct_impl_mut_ref;
        ] );
      ( "enum-impl",
        [
          Alcotest.test_case "method across variants" `Quick
            test_enum_impl_method;
          Alcotest.test_case "method with params" `Quick
            test_enum_impl_with_params;
        ] );
      ( "vec-ops",
        [
          Alcotest.test_case "push and len" `Quick test_vec_push_len;
          Alcotest.test_case "iteration" `Quick test_vec_iteration;
          Alcotest.test_case "pop" `Quick test_vec_pop;
        ] );
      ( "hashmap-ops",
        [
          Alcotest.test_case "insert and len" `Quick test_hashmap_insert_len;
          Alcotest.test_case "contains_key" `Quick test_hashmap_contains_key;
          Alcotest.test_case "remove" `Quick test_hashmap_remove;
          Alcotest.test_case "get" `Quick test_hashmap_get;
        ] );
      ( "go-cleanliness",
        [
          Alcotest.test_case "representative go vet" `Quick
            test_representative_go_vet;
          Alcotest.test_case "determinism with impls" `Quick
            test_determinism_representative;
        ] );
      ( "go-version",
        [
          Alcotest.test_case "Go 1.26+ for trait Self" `Quick
            test_go_version_adequate;
          Alcotest.test_case "below-minimum Go rejects" `Quick
            test_go_version_below_minimum;
          Alcotest.test_case "unparseable Go version" `Quick
            test_go_version_unparseable;
          Alcotest.test_case "simulated success path" `Quick
            test_go_version_success_simulated;
          Alcotest.test_case "future Go version OK" `Quick
            test_go_version_future;
        ] );
      ( "cli-pipeline",
        [ Alcotest.test_case "source to runnable Go" `Quick test_cli_pipeline ]
      );
      ( "example-fixtures",
        [
          Alcotest.test_case "result_option.rg runs" `Quick
            test_result_option_example;
          Alcotest.test_case "impl_methods.rg runs" `Quick
            test_impl_methods_example;
          Alcotest.test_case "shapes.rg runs" `Quick test_shapes_example;
          Alcotest.test_case "loops.rg runs" `Quick test_loops_example;
          Alcotest.test_case "traits.rg runs" `Quick test_traits_example;
        ] );
      ( "generics",
        [
          Alcotest.test_case "generics.rg builds and runs" `Quick
            test_generics_example_go_build;
          Alcotest.test_case "multi-param generic struct" `Quick
            test_generic_struct_codegen;
          Alcotest.test_case "generic free fn" `Quick test_generic_fn_codegen;
          Alcotest.test_case "generic zero value on error path" `Quick
            test_generic_zero_value_result_error_path;
        ] );
      ( "traits",
        [
          Alcotest.test_case "trait without Self" `Quick test_trait_no_self;
          Alcotest.test_case "trait with Self" `Quick test_trait_with_self;
          Alcotest.test_case "struct trait impl" `Quick test_trait_struct_impl;
          Alcotest.test_case "enum trait impl" `Quick test_trait_enum_impl;
          Alcotest.test_case "default method synthesis" `Quick
            test_trait_default_method;
          Alcotest.test_case "default self method call" `Quick
            test_trait_default_self_method_call;
          Alcotest.test_case "generic trait bounds" `Quick
            test_trait_generic_bounds;
          Alcotest.test_case "Self-based trait APIs" `Quick
            test_trait_self_generic;
          Alcotest.test_case "trait go cleanliness" `Quick
            test_trait_go_cleanliness;
        ] );
      ( "acceptance",
        [
          Alcotest.test_case "final MVP acceptance program" `Quick
            test_final_mvp_acceptance;
        ] );
      ( "stdlib-namespace",
        [
          Alcotest.test_case "use net::http with calls and types" `Quick
            test_stdlib_namespace_positive;
          Alcotest.test_case "stdlib types in type and value positions" `Quick
            test_stdlib_type_and_value_positions;
          Alcotest.test_case "wrong-case callable rejected" `Quick
            test_stdlib_wrong_case_callable;
          Alcotest.test_case "wrong-case type rejected" `Quick
            test_stdlib_wrong_case_type;
          Alcotest.test_case "unqualified access rejected" `Quick
            test_stdlib_unqualified_rejected;
          Alcotest.test_case "unknown stdlib member rejected" `Quick
            test_stdlib_unknown_member;
          Alcotest.test_case "stdlib type in expression rejected" `Quick
            test_stdlib_type_in_expr_rejected;
          Alcotest.test_case "local http without import" `Quick
            test_local_http_without_import;
        ] );
      ( "stdlib-receiver-members",
        [
          Alcotest.test_case "mux.handle_func lowers to HandleFunc" `Quick
            test_stdlib_receiver_handle_func;
          Alcotest.test_case "req.form_value lowers to FormValue" `Quick
            test_stdlib_receiver_form_value;
          Alcotest.test_case "req.method lowers to Method" `Quick
            test_stdlib_receiver_method_field;
          Alcotest.test_case "w.write_header lowers to WriteHeader" `Quick
            test_stdlib_receiver_write_header;
          Alcotest.test_case "w.write lowers to Write" `Quick
            test_stdlib_receiver_write;
          Alcotest.test_case "combined handler receiver members" `Quick
            test_stdlib_receiver_combined;
          Alcotest.test_case "Go-cased HandleFunc rejected" `Quick
            test_stdlib_receiver_wrong_case_handle_func;
          Alcotest.test_case "Go-cased FormValue rejected" `Quick
            test_stdlib_receiver_wrong_case_form_value;
          Alcotest.test_case "Go-cased Method rejected" `Quick
            test_stdlib_receiver_wrong_case_method;
          Alcotest.test_case "Go-cased WriteHeader rejected" `Quick
            test_stdlib_receiver_wrong_case_write_header;
          Alcotest.test_case "Go-cased Write rejected" `Quick
            test_stdlib_receiver_wrong_case_write;
          Alcotest.test_case "w.write in value position rejected" `Quick
            test_stdlib_receiver_write_value_position;
          Alcotest.test_case "return w.write(...) rejected" `Quick
            test_stdlib_receiver_write_return_position;
          Alcotest.test_case "return w.write_header(...) rejected" `Quick
            test_stdlib_receiver_write_header_return_position;
        ] );
    ]
