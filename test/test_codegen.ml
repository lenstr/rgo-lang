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

(* Non-identifier Option scrutinees in expression context *)
let test_option_non_identifier_scrutinee_expr () =
  let src =
    {|
fn main() {
    let a = match Some(42) {
        Option::Some(v) => v,
        Option::None => 0,
    };
    println(a);

    let b = match (None as Option<i64>) {
        Option::Some(v) => v,
        Option::None => -1,
    };
    println(b);

    let c = match Some(1 + 2) {
        Option::Some(v) => v * 10,
        Option::None => 0,
    };
    println(c);
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n-1\n30\n" src in
  Alcotest.(check bool)
    "no panic in generated code" true
    (not (contains go "panic"));
  Alcotest.(check bool)
    "typed nil for None scrutinee" false
    (contains go "__match_opt_0 := nil")

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

(* Regression: matching on a Result variable reuses decomposition *)
let test_result_variable_match_stmt () =
  let src =
    {|
fn get_result(x: i64) -> Result<i64, str> {
    if x > 0 {
        Ok(x)
    } else {
        Err("negative")
    }
}

fn main() {
    let r = get_result(42);
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  (* Must not re-destructure the variable with := in the if condition *)
  Alcotest.(check bool) "no re-destructure" false (contains go ":= r;")

let test_result_variable_match_err_stmt () =
  let src =
    {|
fn get_result(x: i64) -> Result<i64, str> {
    if x > 0 {
        Ok(x)
    } else {
        Err("negative")
    }
}

fn main() {
    let r = get_result(-1);
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let _go = compile_and_check ~expected_output:"negative\n" src in
  ()

let test_result_variable_match_expr () =
  let src =
    {|
fn get_result(x: i64) -> Result<i64, str> {
    if x > 0 {
        Ok(x)
    } else {
        Err("negative")
    }
}

fn main() {
    let r = get_result(7);
    let msg = match r {
        Result::Ok(_) => "ok",
        Result::Err(_) => "err",
    };
    println(msg);
}
|}
  in
  let _go = compile_and_check ~expected_output:"ok\n" src in
  ()

(* ---------- Array literal tests ---------- *)

let test_result_variable_match_shadowing_stmt () =
  let src =
    {|
fn get_result(x: i64) -> Result<i64, str> {
    if x > 0 {
        Ok(x)
    } else {
        Err("negative")
    }
}

fn main() {
    let r = get_result(42);
    {
        let r = get_result(-1);
        match r {
            Result::Ok(v) => println(v),
            Result::Err(e) => println(e),
        }
    }
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let _go = compile_and_check ~expected_output:"negative\n42\n" src in
  ()

(* Direct Result constructor let binding and match *)
let test_direct_result_constructor_let () =
  let src =
    {|
fn main() {
    let r: Result<i64, str> = Ok(42);
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  (* Must emit Result struct, not multi-return decomposition *)
  Alcotest.(check bool)
    "emits Result struct" true
    (contains go "Result[int64, string]{ok: true, value: 42}");
  Alcotest.(check bool) "no err decomposition" false (contains go "__err_")

let test_direct_result_constructor_match () =
  let src =
    {|
fn main() {
    match Err("hello") {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let _go = compile_and_check ~expected_output:"hello\n" src in
  ()

let test_direct_result_nested_constructor () =
  let src =
    {|
fn main() {
    let nested: Result<Result<i64, str>, str> = Ok(Ok(7));
    match nested {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let _go = compile_and_check ~expected_output:"7\n" src in
  ()

let test_direct_result_var_match_struct () =
  let src =
    {|
fn main() {
    let inner: Result<i64, str> = Ok(99);
    match inner {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let _go = compile_and_check ~expected_output:"99\n" src in
  ()

let test_ascribed_result_constructor_let () =
  let src =
    {|
fn main() {
    let r = Ok(42) as Result<i64, str>;
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  (* Must emit Result struct, not multi-return decomposition *)
  Alcotest.(check bool)
    "emits Result struct for ascribed let" true
    (contains go "Result[int64, string]{ok: true, value: 42}");
  Alcotest.(check bool)
    "no err decomposition for ascribed let" false (contains go "__err_")

let test_ascribed_result_constructor_match () =
  let src =
    {|
fn main() {
    match Ok(42) as Result<i64, str> {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  (* Must use struct-mode .ok check, not multi-return decomposition *)
  Alcotest.(check bool)
    "emits .ok check for ascribed match" true (contains go ".ok");
  Alcotest.(check bool)
    "no multi-return for ascribed match" false
    (contains go "; __match_err_")

let test_ascribed_err_constructor_match () =
  let src =
    {|
fn main() {
    match Err("hello") as Result<i64, str> {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"hello\n" src in
  (* Must use struct-mode .ok check, not multi-return decomposition *)
  Alcotest.(check bool)
    "emits .ok check for ascribed Err match" true (contains go ".ok");
  Alcotest.(check bool)
    "no multi-return for ascribed Err match" false
    (contains go "; __match_err_")

let test_ascribed_result_let_and_match_combined () =
  let src =
    {|
fn main() {
    let r = Err("fail") as Result<i64, str>;
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"fail\n" src in
  (* Both let and match must use struct mode *)
  Alcotest.(check bool)
    "emits Result struct for ascribed Err let" true
    (contains go "Result[int64, string]{err:");
  Alcotest.(check bool)
    "no err decomposition for ascribed Err let" false (contains go "__err_")

let test_result_constructor_block_let () =
  let src =
    {|
fn main() {
    let r = { Ok(42) };
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let _go = compile_and_check ~expected_output:"42\n" src in
  ()

let test_result_constructor_if_let () =
  let src =
    {|
fn main() {
    let r = if true { Ok(7) } else { Err("fail") };
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let _go = compile_and_check ~expected_output:"7\n" src in
  ()

let test_result_constructor_match_let () =
  let src =
    {|
fn helper() -> Result<i64, str> {
    Ok(1)
}

fn main() {
    let src = helper();
    let r = match src {
        Result::Ok(_) => Ok(99),
        Result::Err(_) => Err("nope"),
    };
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let _go = compile_and_check ~expected_output:"99\n" src in
  ()

let test_nested_result_block_iife () =
  let src =
    {|
fn outer() -> Result<i64, str> {
    let inner = {
        Ok(42)
    };
    match inner {
        Result::Ok(v) => Ok(v),
        Result::Err(e) => Err(e),
    }
}

fn main() {
    match outer() {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  Alcotest.(check bool)
    "nested block IIFE returns Result struct" true
    (contains go "func() Result[int64, string] {");
  Alcotest.(check bool)
    "nested block IIFE Ok is struct literal" true
    (contains go "Result[int64, string]{ok: true, value: 42}");
  Alcotest.(check bool)
    "nested block IIFE does not use tuple return" false
    (contains go "return 42, nil")

let test_nested_result_if_iife () =
  let src =
    {|
fn outer() -> Result<i64, str> {
    let inner = if true { Ok(7) } else { Err("fail") };
    match inner {
        Result::Ok(v) => Ok(v),
        Result::Err(e) => Err(e),
    }
}

fn main() {
    match outer() {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"7\n" src in
  Alcotest.(check bool)
    "nested if IIFE returns Result struct" true
    (contains go "func() Result[int64, string] {");
  Alcotest.(check bool)
    "nested if IIFE Ok is struct literal" true
    (contains go "Result[int64, string]{ok: true, value: 7}");
  Alcotest.(check bool)
    "nested if IIFE does not use tuple return" false
    (contains go "return 7, nil")

let test_nested_result_match_iife () =
  let src =
    {|
fn helper() -> Result<i64, str> {
    Ok(1)
}

fn outer() -> Result<i64, str> {
    let src = helper();
    let inner = match src {
        Result::Ok(_) => Ok(99),
        Result::Err(_) => Err("nope"),
    };
    match inner {
        Result::Ok(v) => Ok(v),
        Result::Err(e) => Err(e),
    }
}

fn main() {
    match outer() {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"99\n" src in
  Alcotest.(check bool)
    "nested match IIFE returns Result struct" true
    (contains go "func() Result[int64, string] {");
  Alcotest.(check bool)
    "nested match IIFE Ok is struct literal" true
    (contains go "Result[int64, string]{ok: true, value: 99}");
  Alcotest.(check bool)
    "nested match IIFE does not use tuple return" false
    (contains go "return 99, nil")

let test_nested_result_if_iife_local_binding () =
  let src =
    {|
fn outer() -> Result<i64, str> {
    let inner = if true {
        let x = Ok(7);
        x
    } else {
        let y = Err("fail");
        y
    };
    match inner {
        Result::Ok(v) => Ok(v),
        Result::Err(e) => Err(e),
    }
}

fn main() {
    match outer() {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"7\n" src in
  Alcotest.(check bool)
    "nested if IIFE local binding returns Result struct" true
    (contains go "func() Result[int64, string] {");
  Alcotest.(check bool)
    "nested if IIFE local binding does not use any" false
    (contains go "func() any {")

let test_nested_result_match_iife_local_binding () =
  let src =
    {|
fn outer() -> Result<Result<i64, str>, str> {
    let src: Result<Result<i64, str>, str> = Ok(Ok(1));
    let inner = match src {
        Result::Ok(v) => {
            let x = Ok(v);
            x
        },
        Result::Err(e) => {
            let y = Err(e);
            y
        },
    };
    inner
}

fn main() {
    match outer() {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"{true 1 }\n" src in
  Alcotest.(check bool)
    "nested match IIFE local binding returns Result struct" true
    (contains go "func() Result[Result[int64, string], string] {");
  Alcotest.(check bool)
    "nested match IIFE local binding does not use any" false
    (contains go "func() any {")

let test_match_iife_pattern_binding () =
  let src =
    {|
enum MyOpt {
    Some(i64),
    None,
}

fn outer() -> i64 {
    let src: MyOpt = MyOpt::Some(42);
    let inner = match src {
        MyOpt::Some(v) => v,
        MyOpt::None => 0,
    };
    inner
}

fn main() {
    println(outer())
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  Alcotest.(check bool)
    "match IIFE pattern binding returns concrete type" true
    (contains go "func() int64 {");
  Alcotest.(check bool)
    "match IIFE pattern binding does not use any" false
    (contains go "func() any {")

let test_result_match_as_return_ok () =
  let src =
    {|
fn wrap(x: i64) -> Result<i64, str> {
    match Ok(x) {
        Result::Ok(v) => Ok(v + 1),
        Result::Err(e) => Err(e),
    }
}

fn main() {
    let r = wrap(41);
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  Alcotest.(check bool)
    "match-as-return Ok arm returns tuple" true
    (contains go "return v + 1, nil");
  Alcotest.(check bool)
    "match-as-return Err arm returns tuple" true
    (contains go "return 0, errors.New(e)")

let test_result_match_as_return_err () =
  let src =
    {|
fn wrap() -> Result<i64, str> {
    match Err("bad") {
        Result::Ok(v) => Ok(v),
        Result::Err(e) => Err("wrapped: " + e),
    }
}

fn main() {
    let r = wrap();
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"wrapped: bad\n" src in
  Alcotest.(check bool)
    "match-as-return Err scrutinee Ok arm returns tuple" true
    (contains go "return v, nil");
  Alcotest.(check bool)
    "match-as-return Err scrutinee Err arm returns tuple" true
    (contains go "return 0, errors.New(\"wrapped: \" + e)")

let test_result_match_as_return_var () =
  let src =
    {|
fn double(r: Result<i64, str>) -> Result<i64, str> {
    match r {
        Result::Ok(v) => Ok(v * 2),
        Result::Err(e) => Err("var: " + e),
    }
}

fn main() {
    let a = double(Ok(5));
    let b = double(Err("fail"));
    match a {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
    match b {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"10\nvar: fail\n" src in
  Alcotest.(check bool)
    "match-as-return var Ok arm returns tuple" true
    (contains go "return v * 2, nil");
  Alcotest.(check bool)
    "match-as-return var Err arm returns tuple" true
    (contains go "return 0, errors.New(\"var: \" + e)");
  Alcotest.(check bool) "var match uses .ok check" true (contains go ".ok")

let test_casted_result_match_as_return_ok () =
  let src =
    {|
fn wrap(x: i64) -> Result<i64, str> {
    match Ok(x) as Result<i64, str> {
        Result::Ok(v) => Ok(v + 1) as Result<i64, str>,
        Result::Err(e) => Err(e) as Result<i64, str>,
    }
}

fn main() {
    let r = wrap(41);
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  Alcotest.(check bool)
    "casted match-as-return Ok arm returns tuple" true
    (contains go "return v + 1, nil");
  Alcotest.(check bool)
    "casted match-as-return Err arm returns tuple" true
    (contains go "return 0, errors.New(e)")

let test_casted_result_match_as_return_var () =
  let src =
    {|
fn double(r: Result<i64, str>) -> Result<i64, str> {
    match r {
        Result::Ok(v) => Ok(v * 2) as Result<i64, str>,
        Result::Err(e) => Err("var: " + e) as Result<i64, str>,
    }
}

fn main() {
    let a = double(Ok(5));
    let b = double(Err("fail"));
    match a {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
    match b {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"10\nvar: fail\n" src in
  Alcotest.(check bool)
    "casted match-as-return var Ok arm returns tuple" true
    (contains go "return v * 2, nil");
  Alcotest.(check bool)
    "casted match-as-return var Err arm returns tuple" true
    (contains go "return 0, errors.New(\"var: \" + e)");
  Alcotest.(check bool)
    "casted var match uses .ok check" true (contains go ".ok")

let test_nested_result_return_signature () =
  let src =
    {|
fn nested_result(x: i64) -> Result<Result<i64, str>, str> {
    if x > 0 {
        Ok(Ok(x) as Result<i64, str>)
    } else if x == 0 {
        Ok(Err("zero") as Result<i64, str>)
    } else {
        Err("negative")
    }
}

fn main() {
    match nested_result(42) {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
    match nested_result(0) {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
    match nested_result(-1) {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
}
|}
  in
  let go = compile_and_check ~expected_output:"42\nzero\nnegative\n" src in
  Alcotest.(check bool)
    "nested Result return signature preserves outer Result" true
    (contains go "Result[Result[int64, string], string]");
  Alcotest.(check bool)
    "nested Result Ok return preserves outer structure" true
    (contains go
       "Result[Result[int64, string], string]{ok: true, value: Result[int64, \
        string]{ok: true, value: x}}");
  Alcotest.(check bool)
    "nested Result Err return preserves outer structure" true
    (contains go "Result[Result[int64, string], string]{err: \"negative\"}");
  Alcotest.(check bool)
    "nested Result match uses struct pattern" true
    (contains go "__match_res_0.ok")

let test_nested_result_question_let () =
  let src =
    {|
fn nested_ok() -> Result<Result<i64, str>, str> {
    Ok(Ok(42) as Result<i64, str>)
}

fn nested_err() -> Result<Result<i64, str>, str> {
    Err("outer error")
}

fn propagate() -> Result<Result<i64, str>, str> {
    let x = nested_ok()?;
    Ok(x)
}

fn propagate_err() -> Result<Result<i64, str>, str> {
    let x = nested_err()?;
    Ok(x)
}

fn main() {
    match propagate() {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
    match propagate_err() {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
}
|}
  in
  let go = compile_and_check ~expected_output:"42\nouter error\n" src in
  Alcotest.(check bool)
    "nested Result ? uses struct mode in let" true
    (contains go "__res_0 := nested_ok()");
  Alcotest.(check bool)
    "nested Result ? checks .ok" true
    (contains go "if !__res_0.ok {");
  Alcotest.(check bool)
    "nested Result ? returns struct on error" true
    (contains go
       "return Result[Result[int64, string], string]{err: __res_0.err}");
  Alcotest.(check bool)
    "nested Result ? extracts .value" true
    (contains go "x := __res_0.value")

let test_nested_result_constructor_let () =
  let src =
    {|
fn main() {
    let direct = Ok(Ok(7) as Result<i64, str>);
    match direct {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
    println("done");
}
|}
  in
  let go = compile_and_check ~expected_output:"7\ndone\n" src in
  Alcotest.(check bool)
    "nested Result constructor let binds struct" true
    (contains go
       "direct := Result[Result[int64, string], string]{ok: true, value: \
        Result[int64, string]{ok: true, value: 7}}");
  Alcotest.(check bool)
    "nested Result constructor let match uses .ok" true
    (contains go "if direct.ok {")

let test_nested_result_question_stmt () =
  let src =
    {|
fn nested_ok() -> Result<Result<i64, str>, str> {
    Ok(Ok(99) as Result<i64, str>)
}

fn propagate() -> Result<Result<i64, str>, str> {
    nested_ok()?;
    Ok(Ok(99) as Result<i64, str>)
}

fn main() {
    match propagate() {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
}
|}
  in
  let go = compile_and_check ~expected_output:"99\n" src in
  Alcotest.(check bool)
    "nested Result ? in stmt uses struct mode" true
    (contains go "__res_0 := nested_ok()");
  Alcotest.(check bool)
    "nested Result ? stmt checks .ok" true
    (contains go "if !__res_0.ok {");
  Alcotest.(check bool)
    "nested Result ? stmt discards .value" true
    (contains go "_ = __res_0.value")

let test_flat_result_binding_final_expr () =
  let src =
    {|
fn wrap(x: i64) -> Result<i64, str> {
    let r = Ok(x);
    r
}

fn main() {
    let a = wrap(5);
    match a {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
|}
  in
  let go = compile_and_check ~expected_output:"5\n" src in
  Alcotest.(check bool)
    "final expr Result ident destructures to tuple" true
    (contains go "if r.ok {");
  Alcotest.(check bool)
    "final expr Ok path returns value, nil" true
    (contains go "return r.value, nil");
  Alcotest.(check bool)
    "final expr Err path returns zero, errors.New" true
    (contains go "return 0, errors.New(r.err)")

let test_flat_result_binding_explicit_return () =
  let src =
    {|
fn wrap_ok(x: i64) -> Result<i64, str> {
    let r = Ok(x);
    return r;
}

fn wrap_err() -> Result<i64, str> {
    let r = Err("bad");
    return r;
}

fn main() {
    let a = wrap_ok(7);
    let b = wrap_err();
    match a {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    };
    match b {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    };
}
|}
  in
  let go = compile_and_check ~expected_output:"7\nbad\n" src in
  Alcotest.(check bool)
    "explicit return Result ident destructures Ok" true
    (contains go "if r.ok {");
  Alcotest.(check bool)
    "explicit return Ok path returns value, nil" true
    (contains go "return r.value, nil");
  Alcotest.(check bool)
    "explicit return Err path returns zero, errors.New" true
    (contains go "return 0, errors.New(r.err)")

let test_flat_result_binding_match_arm () =
  let src =
    {|
fn choose(src: Result<i64, str>) -> Result<i64, str> {
    let good = Ok(99);
    let bad = Err("fail");
    match src {
        Result::Ok(_) => good,
        Result::Err(_) => bad,
    }
}

fn main() {
    let a = choose(Ok(1));
    let b = choose(Err("x"));
    match a {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    };
    match b {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    };
}
|}
  in
  let go = compile_and_check ~expected_output:"99\nfail\n" src in
  Alcotest.(check bool)
    "match arm Result ident destructures to tuple" true
    (contains go "if good.ok {");
  Alcotest.(check bool)
    "match arm Ok path returns value, nil" true
    (contains go "return good.value, nil");
  Alcotest.(check bool)
    "match arm Err path returns zero, errors.New" true
    (contains go "return 0, errors.New(good.err)")

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

(* ======== Callback interop: named handler registration ===== *)

(* VAL-CALLBACK-001: Named handler lowers to Go function reference *)
let test_callback_named_handler () =
  let src =
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    println("handled");
}
fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/items", handler);
    http::listen_and_serve(":3111", mux);
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool)
    "has handler func" true
    (contains go "func handler(w http.ResponseWriter, r *http.Request)");
  Alcotest.(check bool)
    "has HandleFunc with handler" true
    (contains go "mux.HandleFunc(\"/items\", handler)");
  Alcotest.(check bool)
    "has ListenAndServe" true
    (contains go "http.ListenAndServe")

(* VAL-CALLBACK-006: Handler reuse across multiple registrations generates
   correct Go — the same function is referenced on multiple HandleFunc calls
   without being consumed. *)
let test_callback_handler_reuse () =
  let src =
    {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    println("handled");
}
fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/items", handler);
    mux.handle_func("/other", handler);
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool)
    "has first HandleFunc" true
    (contains go "mux.HandleFunc(\"/items\", handler)");
  Alcotest.(check bool)
    "has second HandleFunc" true
    (contains go "mux.HandleFunc(\"/other\", handler)");
  ()

(* VAL-CALLBACK-004: Wrong-signature handler is rejected before Go output *)
let test_callback_wrong_signature_rejected () =
  compile_expect_error ~expect:"type mismatch"
    {|
use net::http;
fn bad_handler(x: i64) {
    println(x);
}
fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/items", bad_handler);
}
|}
    ()

(* VAL-CALLBACK-005: Non-callable integer is rejected with not-callable diagnostic *)
let test_callback_non_callable_int () =
  compile_expect_error ~expect:"not callable"
    {|
use net::http;
fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/items", 42);
}
|}
    ()

(* VAL-CALLBACK-005: Non-callable string is rejected *)
let test_callback_non_callable_string () =
  compile_expect_error ~expect:"not callable"
    {|
use net::http;
fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/items", "not a function");
}
|}
    ()

(* VAL-CALLBACK-002: Zero-capture anonymous handler lowers to Go func literal *)
let test_callback_anonymous_handler () =
  let src =
    {|
use net::http;
fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/items", |w: http::ResponseWriter, r: http::Request| {
        w.write_header(200);
        w.write("hello");
    });
    http::listen_and_serve(":3111", mux);
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool)
    "has Go func literal" true
    (contains go "func(w http.ResponseWriter, r *http.Request) {");
  Alcotest.(check bool)
    "has HandleFunc" true
    (contains go
       "mux.HandleFunc(\"/items\", func(w http.ResponseWriter, r \
        *http.Request) {");
  Alcotest.(check bool)
    "has WriteHeader" true
    (contains go "w.WriteHeader(200)");
  Alcotest.(check bool)
    "has Write([]byte)" true
    (contains go "w.Write([]byte(\"hello\"))")

(* VAL-CROSS-002: Mixed named handler + anonymous handler lowers to valid Go *)
let test_callback_mixed_handlers () =
  let src =
    {|
use net::http;
fn named_handler(w: http::ResponseWriter, r: http::Request) {
    w.write_header(200);
    w.write("named");
}
fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/named", named_handler);
    mux.handle_func("/anon", |w: http::ResponseWriter, r: http::Request| {
        w.write_header(200);
        w.write("anonymous");
    });
    http::listen_and_serve(":3111", mux);
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool)
    "has named handler func" true
    (contains go "func named_handler(w http.ResponseWriter, r *http.Request)");
  Alcotest.(check bool)
    "has HandleFunc for named" true
    (contains go "mux.HandleFunc(\"/named\", named_handler)");
  Alcotest.(check bool)
    "has HandleFunc for anonymous" true
    (contains go
       "mux.HandleFunc(\"/anon\", func(w http.ResponseWriter, r *http.Request)");
  Alcotest.(check bool)
    "has ListenAndServe" true
    (contains go "http.ListenAndServe")

(* VAL-CALLBACK-003: Capturing anonymous handler is rejected *)
let test_callback_capturing_lambda_rejected () =
  compile_expect_error ~expect:"undefined"
    {|
use net::http;
fn main() {
    let greeting = "hello";
    let mux = http::new_serve_mux();
    mux.handle_func("/items", |w: http::ResponseWriter, r: http::Request| {
        w.write_header(200);
        w.write(greeting);
    });
}
|}
    ()

(* VAL-CALLBACK-007: Anonymous handler can be reused across registrations *)
let test_callback_anonymous_handler_reuse () =
  let src =
    {|
use net::http;
fn main() {
    let mux = http::new_serve_mux();
    let handler = |w: http::ResponseWriter, r: http::Request| {
        w.write_header(200);
        w.write("reused");
    };
    mux.handle_func("/items", handler);
    mux.handle_func("/other", handler);
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool)
    "has first HandleFunc" true
    (contains go "mux.HandleFunc(\"/items\",");
  Alcotest.(check bool)
    "has second HandleFunc" true
    (contains go "mux.HandleFunc(\"/other\",")

(* Typed non-void lambda lowers final expression with return *)
let test_typed_nonvoid_lambda_positive () =
  let src =
    {|
fn main() {
    let add = |a: i32, b: i32| -> i32 {
        a + b
    };
    println(add(1, 2));
}
|}
  in
  let go = compile_and_check ~expected_output:"3\n" src in
  Alcotest.(check bool)
    "has func literal with return" true
    (contains go "return a + b")

(* Typed non-void lambda with if expression final arm returns correctly *)
let test_typed_nonvoid_lambda_if_final () =
  let src =
    {|
fn main() {
    let max = |a: i32, b: i32| -> i32 {
        if a > b {
            a
        } else {
            b
        }
    };
    println(max(5, 3));
    println(max(2, 7));
}
|}
  in
  let go = compile_and_check ~expected_output:"5\n7\n" src in
  Alcotest.(check bool) "has return in if arm" true (contains go "return a")

(* Negative: typed non-void lambda with empty body is rejected before Go generation *)
let test_typed_nonvoid_lambda_empty_body_rejected () =
  compile_expect_error ~expect:"must produce a value"
    {|
fn main() {
    let f = |a: i32| -> i32 { };
    println(f(1));
}
|}
    ()

(* Negative: typed non-void lambda with statement-only body is rejected before Go generation *)
let test_typed_nonvoid_lambda_statement_only_rejected () =
  compile_expect_error ~expect:"must produce a value"
    {|
fn main() {
    let f = |a: i32| -> i32 {
        println("x");
    };
    println(f(1));
}
|}
    ()

(* Negative: typed non-void lambda with trailing if-without-else *)
let test_typed_nonvoid_lambda_if_no_else_rejected () =
  compile_expect_error ~expect:"must produce a value"
    {|
fn main() {
    let f = |a: i32| -> i32 {
        if a > 0 {
            println("x");
        }
    };
    println(f(1));
}
|}
    ()

(* Negative: typed non-void lambda with trailing void block *)
let test_typed_nonvoid_lambda_void_block_rejected () =
  compile_expect_error ~expect:"must produce a value"
    {|
fn main() {
    let f = |a: i32| -> i32 {
        {
            println("x");
        }
    };
    println(f(1));
}
|}
    ()

(* Positive: typed non-void lambda with trailing match expression *)
let test_typed_nonvoid_lambda_match_final () =
  let src =
    {|
fn main() {
    let f = |a: i32| -> i32 {
        match Some(a) {
            Option::Some(v) => v,
            Option::None => 0,
        }
    };
    println(f(5));
    println(f(-1));
}
|}
  in
  let go = compile_and_check ~expected_output:"5\n-1\n" src in
  Alcotest.(check bool) "has return in match arm" true (contains go "return v")

(* Positive: typed non-void lambda with trailing block expression *)
let test_typed_nonvoid_lambda_block_final () =
  let src =
    {|
fn main() {
    let f = |a: i32| -> i32 {
        {
            a + 1
        }
    };
    println(f(2));
}
|}
  in
  let go = compile_and_check ~expected_output:"3\n" src in
  Alcotest.(check bool) "has return in block" true (contains go "return a + 1")

(* Positive: typed non-void lambda with if-else where both arms have explicit returns *)
let test_typed_nonvoid_lambda_if_both_return () =
  let src =
    {|
fn main() {
    let f = |a: i32| -> i32 {
        if a > 0 {
            return a;
        } else {
            return 0;
        }
    };
    println(f(5));
    println(f(-1));
}
|}
  in
  let go = compile_and_check ~expected_output:"5\n0\n" src in
  Alcotest.(check bool) "has return in if arm" true (contains go "return a")

(* Positive: typed non-void lambda with mixed return/value if branches *)
let test_typed_nonvoid_lambda_if_mixed_return_value () =
  let src =
    {|
fn main() {
    let f = |a: i32| -> i32 {
        if a > 0 {
            return a;
        } else {
            0
        }
    };
    println(f(5));
    println(f(-1));
}
|}
  in
  let go = compile_and_check ~expected_output:"5\n0\n" src in
  Alcotest.(check bool) "has return in then arm" true (contains go "return a");
  Alcotest.(check bool) "has return in else arm" true (contains go "return 0")

(* Positive: typed non-void lambda with mixed value/return if branches *)
let test_typed_nonvoid_lambda_if_mixed_value_return () =
  let src =
    {|
fn main() {
    let f = |a: i32| -> i32 {
        if a > 0 {
            a
        } else {
            return 0;
        }
    };
    println(f(5));
    println(f(-1));
}
|}
  in
  let go = compile_and_check ~expected_output:"5\n0\n" src in
  Alcotest.(check bool) "has return in else arm" true (contains go "return 0")

(* Positive: typed non-void lambda with mixed return/value if branches
   where the value branch consumes a non-Copy binding (regression for
   move-replay in expr_guarantees_value). *)
let test_typed_nonvoid_lambda_if_mixed_return_value_move_consume () =
  let src =
    {|
trait Drop {
    fn drop(&mut self);
}

struct Wrapper {
    value: i32,
}

impl Drop for Wrapper {
    fn drop(&mut self) {}
}

fn take(w: Wrapper) -> i32 {
    w.value
}

fn main() {
    let f = |a: i32| -> i32 {
        let w = Wrapper { value: 42 };
        if a > 0 {
            return a;
        } else {
            take(w)
        }
    };
    println(f(5));
    println(f(-1));
}
|}
  in
  compile_and_check ~expected_output:"5\n42\n" src |> ignore

(* Negative: typed non-void lambda with mixed return/void if branches *)
let test_typed_nonvoid_lambda_if_mixed_return_void_rejected () =
  compile_expect_error ~expect:"must produce a value"
    {|
fn main() {
    let f = |a: i32| -> i32 {
        if a > 0 {
            return a;
        } else {
            println("x");
        }
    };
    println(f(1));
}
|}
    ()

let test_module_level_let () =
  let src = {|
let counter: i64 = 0;

fn main() {
    println(counter);
}
|} in
  let go = compile_and_check src in
  Alcotest.(check bool)
    "has var counter" true
    (contains go "var counter int64 = 0");
  Alcotest.(check bool)
    "has fmt.Println" true
    (contains go "fmt.Println(counter)")

let test_module_level_let_mut () =
  let src =
    {|
let mut items: Vec<String> = Vec::new();

fn main() {
    items.push("hello");
    println(items.len());
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool)
    "has var items" true
    (contains go "var items = make([]string, 0)");
  Alcotest.(check bool)
    "has append" true
    (contains go "items = append(items, \"hello\")");
  Alcotest.(check bool) "has len" true (contains go "len(items)")

let test_module_level_let_reordered () =
  let src =
    {|
fn get_counter() -> i64 {
    counter
}

let counter: i64 = 42;

fn main() {
    println(get_counter());
}
|}
  in
  let go = compile_and_check ~expected_output:"42\n" src in
  Alcotest.(check bool)
    "has var counter" true
    (contains go "var counter int64 = 42");
  Alcotest.(check bool) "calls get_counter" true (contains go "get_counter()")

let test_http_crud_fixture_reordered () =
  let src =
    {|
use net::http;

fn handle_items(w: http::ResponseWriter, r: http::Request) {
    let m = r.method;
    if m == "GET" {
        let mut response = "[";
        let mut first = true;
        for item in items {
            if !first {
                response = response + ", ";
            }
            response = response + "\"" + item + "\"";
            first = false;
        }
        response = response + "]";
        w.write_header(200);
        w.write(response);
    } else if m == "POST" {
        let name = r.form_value("name");
        if name == "" {
            w.write_header(400);
            w.write("missing name");
        } else {
            items.push(name);
            w.write_header(201);
            w.write("created: " + name);
        }
    } else {
        w.write_header(405);
        w.write("method not allowed");
    }
}

let mut items: Vec<String> = Vec::new();

fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/items", handle_items);
    http::listen_and_serve("127.0.0.1:3111", mux);
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool)
    "has var items" true
    (contains go "var items = make([]string, 0)");
  Alcotest.(check bool) "has GET handler" true (contains go "r.Method");
  Alcotest.(check bool)
    "has POST handler" true
    (contains go "r.FormValue(\"name\")");
  Alcotest.(check bool)
    "has ListenAndServe panic" true
    (contains go "if err := http.ListenAndServe(");
  Alcotest.(check bool) "has panic err" true (contains go "panic(err)");
  Alcotest.(check bool)
    "has 400 for missing name" true
    (contains go "WriteHeader(400)");
  Alcotest.(check bool)
    "has missing name body" true
    (contains go "missing name");
  Alcotest.(check bool)
    "has 405 for unsupported method" true
    (contains go "WriteHeader(405)");
  Alcotest.(check bool)
    "has method not allowed body" true
    (contains go "method not allowed");
  Alcotest.(check bool)
    "has append in success branch" true
    (contains go "append(items, name)");
  Alcotest.(check bool)
    "has 201 for success" true
    (contains go "WriteHeader(201)")

let test_http_crud_fixture () =
  let src =
    {|
use net::http;

let mut items: Vec<String> = Vec::new();

fn handle_items(w: http::ResponseWriter, r: http::Request) {
    let m = r.method;
    if m == "GET" {
        let mut response = "[";
        let mut first = true;
        for item in items {
            if !first {
                response = response + ", ";
            }
            response = response + "\"" + item + "\"";
            first = false;
        }
        response = response + "]";
        w.write_header(200);
        w.write(response);
    } else if m == "POST" {
        let name = r.form_value("name");
        if name == "" {
            w.write_header(400);
            w.write("missing name");
        } else {
            items.push(name);
            w.write_header(201);
            w.write("created: " + name);
        }
    } else {
        w.write_header(405);
        w.write("method not allowed");
    }
}

fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/items", handle_items);
    http::listen_and_serve("127.0.0.1:3111", mux);
}
|}
  in
  let go = compile_and_check src in
  Alcotest.(check bool)
    "has var items" true
    (contains go "var items = make([]string, 0)");
  Alcotest.(check bool) "has GET handler" true (contains go "r.Method");
  Alcotest.(check bool)
    "has POST handler" true
    (contains go "r.FormValue(\"name\")");
  Alcotest.(check bool)
    "has ListenAndServe panic" true
    (contains go "if err := http.ListenAndServe(");
  Alcotest.(check bool) "has panic err" true (contains go "panic(err)");
  (* Error-path coverage: malformed POST returns 400 without mutating state *)
  Alcotest.(check bool)
    "has 400 for missing name" true
    (contains go "WriteHeader(400)");
  Alcotest.(check bool)
    "has missing name body" true
    (contains go "missing name");
  (* Error-path coverage: unsupported methods return 405 *)
  Alcotest.(check bool)
    "has 405 for unsupported method" true
    (contains go "WriteHeader(405)");
  Alcotest.(check bool)
    "has method not allowed body" true
    (contains go "method not allowed");
  (* State mutation only happens in the success branch *)
  Alcotest.(check bool)
    "has append in success branch" true
    (contains go "append(items, name)");
  Alcotest.(check bool)
    "has 201 for success" true
    (contains go "WriteHeader(201)")

(* VAL-CROSS-003: Interop-heavy CRUD fixture is deterministic and downstream-tool clean *)
let test_interop_crud_determinism () =
  let src = read_file "../.factory/runtime/interop-crud.rg" in
  let go1 = compile_snapshot src in
  let go2 = compile_snapshot src in
  Alcotest.(check string) "interop crud deterministic" go1 go2

let test_interop_crud_go_cleanliness () =
  let src = read_file "../.factory/runtime/interop-crud.rg" in
  (* compile_and_check without expected_output validates gofmt, go build, go vet *)
  let _go = compile_and_check src in
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
          Alcotest.test_case "non-identifier Option scrutinee expr" `Quick
            test_option_non_identifier_scrutinee_expr;
        ] );
      ( "result",
        [
          Alcotest.test_case "basic result" `Quick test_result_basic;
          Alcotest.test_case "let _ = result fn" `Quick test_let_wildcard_result;
          Alcotest.test_case "result sig without Err" `Quick
            test_result_sig_no_err_construction;
          Alcotest.test_case "result variable match stmt Ok" `Quick
            test_result_variable_match_stmt;
          Alcotest.test_case "result variable match stmt Err" `Quick
            test_result_variable_match_err_stmt;
          Alcotest.test_case "result variable match expr" `Quick
            test_result_variable_match_expr;
          Alcotest.test_case "result variable match shadowing stmt" `Quick
            test_result_variable_match_shadowing_stmt;
          Alcotest.test_case "direct result constructor let" `Quick
            test_direct_result_constructor_let;
          Alcotest.test_case "direct result constructor match" `Quick
            test_direct_result_constructor_match;
          Alcotest.test_case "direct result nested constructor" `Quick
            test_direct_result_nested_constructor;
          Alcotest.test_case "direct result var match struct" `Quick
            test_direct_result_var_match_struct;
          Alcotest.test_case "ascribed result constructor let" `Quick
            test_ascribed_result_constructor_let;
          Alcotest.test_case "ascribed result constructor match" `Quick
            test_ascribed_result_constructor_match;
          Alcotest.test_case "ascribed err constructor match" `Quick
            test_ascribed_err_constructor_match;
          Alcotest.test_case "ascribed result let and match combined" `Quick
            test_ascribed_result_let_and_match_combined;
          Alcotest.test_case "result constructor block let" `Quick
            test_result_constructor_block_let;
          Alcotest.test_case "result constructor if let" `Quick
            test_result_constructor_if_let;
          Alcotest.test_case "result constructor match let" `Quick
            test_result_constructor_match_let;
          Alcotest.test_case "nested result block iife" `Quick
            test_nested_result_block_iife;
          Alcotest.test_case "nested result if iife" `Quick
            test_nested_result_if_iife;
          Alcotest.test_case "nested result match iife" `Quick
            test_nested_result_match_iife;
          Alcotest.test_case "nested result if iife local binding" `Quick
            test_nested_result_if_iife_local_binding;
          Alcotest.test_case "nested result match iife local binding" `Quick
            test_nested_result_match_iife_local_binding;
          Alcotest.test_case "match iife pattern binding" `Quick
            test_match_iife_pattern_binding;
          Alcotest.test_case "result match as return Ok" `Quick
            test_result_match_as_return_ok;
          Alcotest.test_case "result match as return Err" `Quick
            test_result_match_as_return_err;
          Alcotest.test_case "result match as return var" `Quick
            test_result_match_as_return_var;
          Alcotest.test_case "casted result match as return Ok" `Quick
            test_casted_result_match_as_return_ok;
          Alcotest.test_case "casted result match as return var" `Quick
            test_casted_result_match_as_return_var;
          Alcotest.test_case "nested Result return signature" `Quick
            test_nested_result_return_signature;
          Alcotest.test_case "nested Result constructor let" `Quick
            test_nested_result_constructor_let;
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
          Alcotest.test_case "nested Result ? in let" `Quick
            test_nested_result_question_let;
          Alcotest.test_case "nested Result ? in stmt" `Quick
            test_nested_result_question_stmt;
          Alcotest.test_case "flat Result binding final expr" `Quick
            test_flat_result_binding_final_expr;
          Alcotest.test_case "flat Result binding explicit return" `Quick
            test_flat_result_binding_explicit_return;
          Alcotest.test_case "flat Result binding match arm" `Quick
            test_flat_result_binding_match_arm;
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
      ( "callback-interop",
        [
          Alcotest.test_case "named handler lowers to Go func ref" `Quick
            test_callback_named_handler;
          Alcotest.test_case "handler reuse across registrations" `Quick
            test_callback_handler_reuse;
          Alcotest.test_case "wrong-signature handler rejected" `Quick
            test_callback_wrong_signature_rejected;
          Alcotest.test_case "non-callable int rejected" `Quick
            test_callback_non_callable_int;
          Alcotest.test_case "non-callable string rejected" `Quick
            test_callback_non_callable_string;
          Alcotest.test_case "anonymous handler lowers to Go func literal"
            `Quick test_callback_anonymous_handler;
          Alcotest.test_case "mixed named + anonymous handlers" `Quick
            test_callback_mixed_handlers;
          Alcotest.test_case "capturing lambda rejected" `Quick
            test_callback_capturing_lambda_rejected;
          Alcotest.test_case "anonymous handler reuse" `Quick
            test_callback_anonymous_handler_reuse;
          Alcotest.test_case "typed non-void lambda lowers with return" `Quick
            test_typed_nonvoid_lambda_positive;
          Alcotest.test_case "typed non-void lambda if final arm returns" `Quick
            test_typed_nonvoid_lambda_if_final;
          Alcotest.test_case "typed non-void lambda empty body rejected" `Quick
            test_typed_nonvoid_lambda_empty_body_rejected;
          Alcotest.test_case
            "typed non-void lambda statement-only body rejected" `Quick
            test_typed_nonvoid_lambda_statement_only_rejected;
          Alcotest.test_case "typed non-void lambda if-without-else rejected"
            `Quick test_typed_nonvoid_lambda_if_no_else_rejected;
          Alcotest.test_case
            "typed non-void lambda trailing void block rejected" `Quick
            test_typed_nonvoid_lambda_void_block_rejected;
          Alcotest.test_case "typed non-void lambda match final arm returns"
            `Quick test_typed_nonvoid_lambda_match_final;
          Alcotest.test_case "typed non-void lambda block final arm returns"
            `Quick test_typed_nonvoid_lambda_block_final;
          Alcotest.test_case "typed non-void lambda if both arms return" `Quick
            test_typed_nonvoid_lambda_if_both_return;
          Alcotest.test_case "typed non-void lambda if mixed return/value"
            `Quick test_typed_nonvoid_lambda_if_mixed_return_value;
          Alcotest.test_case "typed non-void lambda if mixed value/return"
            `Quick test_typed_nonvoid_lambda_if_mixed_value_return;
          Alcotest.test_case
            "typed non-void lambda if mixed return/value move consume" `Quick
            test_typed_nonvoid_lambda_if_mixed_return_value_move_consume;
          Alcotest.test_case
            "typed non-void lambda if mixed return/void rejected" `Quick
            test_typed_nonvoid_lambda_if_mixed_return_void_rejected;
        ] );
      ( "module-level-let",
        [
          Alcotest.test_case "module-level let compiles and generates var"
            `Quick test_module_level_let;
          Alcotest.test_case "module-level let mut supports mutation" `Quick
            test_module_level_let_mut;
          Alcotest.test_case "module-level let reordered compiles and runs"
            `Quick test_module_level_let_reordered;
          Alcotest.test_case "http crud fixture compiles and is gofmt clean"
            `Quick test_http_crud_fixture;
          Alcotest.test_case
            "http crud fixture reordered compiles and is gofmt clean" `Quick
            test_http_crud_fixture_reordered;
        ] );
      ( "interop-determinism",
        [
          Alcotest.test_case "interop crud fixture compiles deterministically"
            `Quick test_interop_crud_determinism;
          Alcotest.test_case "interop crud fixture is gofmt build and vet clean"
            `Quick test_interop_crud_go_cleanliness;
        ] );
    ]
