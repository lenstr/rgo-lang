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

(* Regression: chained method call on Self-returning constructor result *)
let valid_chained_constructor_method =
  {|
struct Counter {
    count: i32,
}
impl Counter {
    fn new() -> Self {
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
    let v = Counter::new().get();
}
|}

let valid_chained_constructor_field =
  {|
struct Point {
    pub x: f64,
    pub y: f64,
}
impl Point {
    fn origin() -> Self {
        Point { x: 0.0, y: 0.0 }
    }
}
fn main() {
    let x = Point::origin().x;
}
|}

let valid_chained_constructor_multi =
  {|
struct Builder {
    val: i32,
}
impl Builder {
    fn new() -> Self {
        Builder { val: 0 }
    }
    fn build(&self) -> i32 {
        self.val
    }
}
fn use_builder() -> i32 {
    Builder::new().build()
}
fn main() {
    let r = use_builder();
}
|}

(* Enum constructor returning Self with chained method call *)
let valid_enum_self_constructor_method =
  {|
enum Status {
    Active,
    Inactive,
}
impl Status {
    fn default() -> Self {
        Status::Active
    }
    fn is_active(&self) -> bool {
        true
    }
}
fn main() {
    let a = Status::default().is_active();
}
|}

(* Regression: stored associated-function value with Self substitution *)
let valid_stored_assoc_fn_self =
  {|
struct Counter {
    count: i32,
}
impl Counter {
    fn new() -> Self {
        Counter { count: 0 }
    }
    fn get(&self) -> i32 {
        self.count
    }
}
fn main() {
    let make = Counter::new;
    let c = make();
    let v = c.get();
}
|}

let valid_stored_enum_assoc_fn_self =
  {|
enum Status {
    Active,
    Inactive,
}
impl Status {
    fn default() -> Self {
        Status::Active
    }
    fn is_active(&self) -> bool {
        true
    }
}
fn main() {
    let make = Status::default;
    let s = make();
    let a = s.is_active();
}
|}

(* Regression: generic Self substitution preserves type parameters *)
let valid_generic_self_substitution =
  {|
struct Box<T> {
    value: T,
}
impl<T> Box<T> {
    pub fn new(v: T) -> Self {
        Box { value: v }
    }
    pub fn get(&self) -> T {
        self.value
    }
}
fn main() {
    let b = Box::new(42);
    let v = b.get();
}
|}

(* Regression: generic Self in chained constructor+method call *)
let valid_generic_self_chained =
  {|
struct Wrapper<T> {
    inner: T,
}
impl<T> Wrapper<T> {
    pub fn wrap(v: T) -> Self {
        Wrapper { inner: v }
    }
    pub fn unwrap(&self) -> T {
        self.inner
    }
}
fn main() {
    let x = Wrapper::wrap("hello").unwrap();
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

(* Regression: generic enum struct variant literal preserves type args in impl *)
let valid_generic_enum_struct_variant =
  {|
enum Container<T> {
    Empty,
    Item { value: T },
}
impl<T> Container<T> {
    pub fn make(v: T) -> Self {
        Container::Item { value: v }
    }
}
fn main() {
    let c = Container::make(42);
}
|}

(* Regression: generic enum struct variant literal in array element position *)
let valid_generic_enum_struct_variant_in_array =
  {|
enum Wrapper<T> {
    None,
    Val { inner: T },
}
impl<T> Wrapper<T> {
    pub fn wrap(v: T) -> Self {
        Wrapper::Val { inner: v }
    }
}
fn main() {
    let a = Wrapper::wrap(1);
    let b = Wrapper::wrap(2);
    let items = [a, b];
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

(* --- Regression: trait impl receiver-kind mismatch --- *)
let trait_recv_ref_vs_value =
  {|
trait Greet {
    fn hello(&self) -> str;
}
struct Bob {}
impl Greet for Bob {
    fn hello(self) -> str {
        "hello"
    }
}
fn main() {}
|}

let trait_recv_ref_vs_mutref =
  {|
trait Greet {
    fn hello(&self) -> str;
}
struct Bob {}
impl Greet for Bob {
    fn hello(&mut self) -> str {
        "hello"
    }
}
fn main() {}
|}

let trait_recv_mutref_vs_ref =
  {|
trait Counter {
    fn inc(&mut self);
}
struct Cnt { val: i32 }
impl Counter for Cnt {
    fn inc(&self) {
        println("noop");
    }
}
fn main() {}
|}

let trait_recv_value_vs_ref =
  {|
trait Consume {
    fn consume(self) -> str;
}
struct Item {}
impl Consume for Item {
    fn consume(&self) -> str {
        "consumed"
    }
}
fn main() {}
|}

(* Positive: all receiver kinds match *)
let valid_trait_recv_value =
  {|
trait Consume {
    fn consume(self) -> str;
}
struct Item {}
impl Consume for Item {
    fn consume(self) -> str {
        "consumed"
    }
}
fn main() {}
|}

let valid_trait_recv_mutref =
  {|
trait Counter {
    fn inc(&mut self);
}
struct Cnt { val: i32 }
impl Counter for Cnt {
    fn inc(&mut self) {
        self.val = self.val + 1;
    }
}
fn main() {}
|}

(* Undefined variant on enum path *)
let undef_variant_enum_path =
  {|
enum Color {
    Red,
    Green,
}
fn main() {
    let c = Color::Blue;
}
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

(* --- Regression: generic type compatibility soundness --- *)

(* Invalid: non-generic function cannot return a generic-typed value as a
   concrete type.  b.get() returns TParam T which is out of scope in main. *)
let invalid_generic_return =
  {|
struct Box<T> {
    value: T,
}
impl<T> Box<T> {
    pub fn new(v: T) -> Self {
        Box { value: v }
    }
    pub fn get(&self) -> T {
        self.value
    }
}
fn main() -> i32 {
    let b = Box::new(42);
    b.get()
}
|}

(* Invalid: assigning generic-typed value to incompatible annotation.
   w.unwrap() returns TParam T which cannot satisfy an explicit str type. *)
let invalid_generic_assign =
  {|
struct Wrapper<T> {
    inner: T,
}
impl<T> Wrapper<T> {
    pub fn wrap(v: T) -> Self {
        Wrapper { inner: v }
    }
    pub fn unwrap(&self) -> T {
        self.inner
    }
}
fn main() {
    let w = Wrapper::wrap(42);
    let x: str = w.unwrap();
}
|}

(* Regression: passing a generic parameter to a concrete-typed parameter
   must be rejected.  T is not known to be i32. *)
let invalid_generic_param_to_concrete =
  {|
fn takes_i32(x: i32) -> i32 {
    x
}
fn foo<T>(x: T) -> i32 {
    takes_i32(x)
}
fn main() {
    let _ = foo(42);
}
|}

(* Regression: returning a different generic parameter where another is
   expected must be rejected.  B is not the same as A. *)
let invalid_generic_param_cross_return =
  {|
fn bad<A, B>(a: A, b: B) -> A {
    b
}
fn main() {
    let _ = bad(1, 2);
}
|}

(* Regression: generic constructor outside impl infers concrete type args.
   Box::new(42) should produce Box<i32>, satisfying the return type. *)
let valid_generic_constructor_outside_impl =
  {|
struct Box<T> {
    value: T,
}
impl<T> Box<T> {
    pub fn new(v: T) -> Self {
        Box { value: v }
    }
}
fn make() -> Box<i32> {
    let b = Box::new(42);
    b
}
fn main() {
    let _ = make();
}
|}

(* Regression: generic pair constructor outside impl infers from multiple args *)
let valid_generic_pair_constructor =
  {|
struct Pair<A, B> {
    first: A,
    second: B,
}
impl<A, B> Pair<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Pair { first: a, second: b }
    }
}
fn make() -> Pair<i32, str> {
    Pair::new(1, "hello")
}
fn main() {
    let _ = make();
}
|}

(* Regression: generic constructor result flows into Option return *)
let valid_generic_constructor_option_return =
  {|
struct Box<T> {
    value: T,
}
impl<T> Box<T> {
    pub fn new(v: T) -> Self {
        Box { value: v }
    }
}
fn wrap() -> Option<Box<i32>> {
    Some(Box::new(42))
}
fn main() {
    let _ = wrap();
}
|}

(* Regression: generic constructor result flows into Result return *)
let valid_generic_constructor_result_return =
  {|
struct Box<T> {
    value: T,
}
impl<T> Box<T> {
    pub fn new(v: T) -> Self {
        Box { value: v }
    }
}
fn wrap() -> Result<Box<i32>, str> {
    Ok(Box::new(42))
}
fn main() {
    let _ = wrap();
}
|}

(* Regression: generic constructor with typed let binding *)
let valid_generic_constructor_typed_let =
  {|
struct Box<T> {
    value: T,
}
impl<T> Box<T> {
    pub fn new(v: T) -> Self {
        Box { value: v }
    }
}
fn main() {
    let b: Box<i32> = Box::new(42);
    let _ = b;
}
|}

(* --- VAL-TRAIT-010: Duplicate trait impls --- *)
let duplicate_trait_impl =
  {|
trait Greet {
  fn hello(&self) -> str;
}

struct Bot { name: str }

impl Greet for Bot {
  fn hello(&self) -> str { self.name }
}

impl Greet for Bot {
  fn hello(&self) -> str { "hi" }
}

fn main() {}
|}

(* --- VAL-TRAIT-011: Bounded generics reject types without impl --- *)
let unsatisfied_trait_bound =
  {|
trait Show {
  fn show(&self) -> str;
}

struct Plain { val: i64 }

fn display<T: Show>(item: T) -> str {
  item.show()
}

fn main() {
  let p = Plain { val: 1 };
  let _ = display(p);
}
|}

(* --- VAL-TRAIT-012: Ambiguous method resolution --- *)
let ambiguous_method =
  {|
trait Greet {
  fn hello(&self) -> str;
}

trait Welcome {
  fn hello(&self) -> str;
}

struct Bot { name: str }

impl Greet for Bot {
  fn hello(&self) -> str { "greet" }
}

impl Welcome for Bot {
  fn hello(&self) -> str { "welcome" }
}

fn main() {
  let b = Bot { name: "R2" };
  let _ = b.hello();
}
|}

(* --- Regression: bounded generic method call works --- *)
let valid_bounded_generic_method =
  {|
trait Show {
  fn show(&self) -> str;
}

struct Msg { text: str }

impl Show for Msg {
  fn show(&self) -> str { self.text }
}

fn display<T: Show>(item: T) -> str {
  item.show()
}

fn main() {
  let m = Msg { text: "ok" };
  let _ = display(m);
}
|}

(* --- Regression: bounded generic ambiguous method --- *)
let bounded_generic_ambiguous_method =
  {|
trait Greet {
  fn hello(&self) -> str;
}

trait Welcome {
  fn hello(&self) -> str;
}

fn say<T: Greet + Welcome>(item: T) -> str {
  item.hello()
}

fn main() {}
|}

(* --- Regression: unconstrained generic cannot call trait method --- *)
let unconstrained_generic_trait_method =
  {|
trait Show {
  fn show(&self) -> str;
}

fn display<T>(item: T) -> str {
  item.show()
}

fn main() {}
|}

(* --- Regression: default trait method body is typechecked --- *)
let invalid_default_method_body =
  {|
trait Broken {
  fn bad(&self) -> i64 {
    "not an int"
  }
}

fn main() {}
|}

(* --- Regression: outer generic bounds from impl are visible --- *)
let valid_outer_bound_impl_method =
  {|
trait Show {
  fn show(&self) -> str;
}

struct Printer<T> { val: T }

impl<T: Show> Printer<T> {
  fn print_val(&self) -> str {
    self.val.show()
  }
}

fn main() {}
|}

(* --- Regression: outer generic bounds from trait impl are visible --- *)
let valid_outer_bound_trait_impl_method =
  {|
trait Show {
  fn show(&self) -> str;
}

trait Wrapper {
  fn describe(&self) -> str;
}

struct MyBox<T> { val: T }

impl<T: Show> Wrapper for MyBox<T> {
  fn describe(&self) -> str {
    self.val.show()
  }
}

fn main() {}
|}

(* --- Regression: outer generic bounds from trait decl are visible in default body --- *)
let valid_outer_bound_trait_default_body =
  {|
trait Show {
  fn show(&self) -> str;
}

trait Formatter<T: Show> {
  fn format(&self, item: T) -> str {
    item.show()
  }
}

fn main() {}
|}

(* --- Regression: default trait method can call sibling trait methods via self --- *)
let valid_default_trait_self_method_call =
  {|
trait Summary {
  fn summary(&self) -> str;
  fn short(&self) -> str {
    self.summary()
  }
}

struct Article {
  pub title: str,
}

impl Summary for Article {
  fn summary(&self) -> str {
    self.title
  }
}

fn main() {}
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
    ("chained constructor method", `Quick, pass valid_chained_constructor_method);
    ("chained constructor field", `Quick, pass valid_chained_constructor_field);
    ("chained constructor multi", `Quick, pass valid_chained_constructor_multi);
    ( "enum Self constructor method",
      `Quick,
      pass valid_enum_self_constructor_method );
    ( "stored assoc fn Self substitution",
      `Quick,
      pass valid_stored_assoc_fn_self );
    ( "stored enum assoc fn Self substitution",
      `Quick,
      pass valid_stored_enum_assoc_fn_self );
    ("generic Self substitution", `Quick, pass valid_generic_self_substitution);
    ("generic Self chained", `Quick, pass valid_generic_self_chained);
    ("string concat", `Quick, pass valid_string_concat);
    ("empty Vec with annotation", `Quick, pass valid_vec_typed_empty);
    ( "generic enum struct variant in impl",
      `Quick,
      pass valid_generic_enum_struct_variant );
    ( "generic enum struct variant in array",
      `Quick,
      pass valid_generic_enum_struct_variant_in_array );
    ("valid trait impl", `Quick, pass valid_trait_impl);
    ("valid trait recv value", `Quick, pass valid_trait_recv_value);
    ("valid trait recv &mut self", `Quick, pass valid_trait_recv_mutref);
    ( "generic constructor outside impl",
      `Quick,
      pass valid_generic_constructor_outside_impl );
    ("generic pair constructor", `Quick, pass valid_generic_pair_constructor);
    ( "generic constructor option return",
      `Quick,
      pass valid_generic_constructor_option_return );
    ( "generic constructor result return",
      `Quick,
      pass valid_generic_constructor_result_return );
    ( "generic constructor typed let",
      `Quick,
      pass valid_generic_constructor_typed_let );
    ("bounded generic method call", `Quick, pass valid_bounded_generic_method);
    ("outer bound impl method", `Quick, pass valid_outer_bound_impl_method);
    ( "outer bound trait impl method",
      `Quick,
      pass valid_outer_bound_trait_impl_method );
    ( "outer bound trait default body",
      `Quick,
      pass valid_outer_bound_trait_default_body );
    ( "default trait self method call",
      `Quick,
      pass valid_default_trait_self_method_call );
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
    ( "undefined variant in enum path",
      `Quick,
      fail ~expect:"undefined variant" undef_variant_enum_path );
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
    ( "trait recv &self vs self mismatch",
      `Quick,
      fail ~expect:"receiver mismatch" trait_recv_ref_vs_value );
    ( "trait recv &self vs &mut self mismatch",
      `Quick,
      fail ~expect:"receiver mismatch" trait_recv_ref_vs_mutref );
    ( "trait recv &mut self vs &self mismatch",
      `Quick,
      fail ~expect:"receiver mismatch" trait_recv_mutref_vs_ref );
    ( "trait recv self vs &self mismatch",
      `Quick,
      fail ~expect:"receiver mismatch" trait_recv_value_vs_ref );
    ( "generic return type mismatch",
      `Quick,
      fail ~expect:"type mismatch" invalid_generic_return );
    ( "generic assign type mismatch",
      `Quick,
      fail ~expect:"type mismatch" invalid_generic_assign );
    ( "generic param to concrete rejected",
      `Quick,
      fail ~expect:"type mismatch" invalid_generic_param_to_concrete );
    ( "generic cross-param return rejected",
      `Quick,
      fail ~expect:"type mismatch" invalid_generic_param_cross_return );
    ( "duplicate trait impl",
      `Quick,
      fail ~expect:"duplicate impl" duplicate_trait_impl );
    ( "unsatisfied trait bound",
      `Quick,
      fail ~expect:"does not implement trait" unsatisfied_trait_bound );
    ( "ambiguous method resolution",
      `Quick,
      fail ~expect:"ambiguous method" ambiguous_method );
    ( "bounded generic ambiguous method",
      `Quick,
      fail ~expect:"ambiguous method" bounded_generic_ambiguous_method );
    ( "unconstrained generic trait method",
      `Quick,
      fail ~expect:"no method" unconstrained_generic_trait_method );
    ( "invalid default method body",
      `Quick,
      fail ~expect:"type mismatch" invalid_default_method_body );
  ]

(* ---- Import-aware typecheck tests ---- *)

let valid_import_passthrough = {|
use net::http;
fn main() {}
|}

let import_unknown_member_value =
  {|
use net::http;
fn main() {
    let x = http::missing_symbol;
}
|}

let import_unknown_member_call =
  {|
use net::http;
fn main() {
    http::missing_fn();
}
|}

(* Positive: stdlib function calls typecheck *)
let valid_stdlib_call =
  {|
use net::http;
fn main() {
    let mux = http::new_serve_mux();
    http::listen_and_serve(":8080", mux);
}
|}

(* Positive: stdlib types in function signatures *)
let valid_stdlib_types =
  {|
use net::http;
fn handle(w: http::ResponseWriter, r: http::Request) {
    println("ok");
}
fn main() {
}
|}

(* Negative: Go-cased callable *)
let wrong_case_callable =
  {|
use net::http;
fn main() {
    let mux = http::NewServeMux();
}
|}

(* Negative: snake_case type *)
let wrong_case_type =
  {|
use net::http;
fn handle(w: http::response_writer, r: http::request) {
    println("fail");
}
fn main() {
}
|}

(* Negative: imported type in expression position *)
let stdlib_type_in_expr =
  {|
use net::http;
fn main() {
    let x = http::Request;
}
|}

(* Negative: imported type as function call *)
let stdlib_type_called =
  {|
use net::http;
fn main() {
    let x = http::Request();
}
|}

let import_positive_tests =
  [
    ( "use net::http without usage passes typecheck",
      `Quick,
      pass valid_import_passthrough );
    ("stdlib function calls typecheck correctly", `Quick, pass valid_stdlib_call);
    ( "stdlib types in signatures typecheck correctly",
      `Quick,
      pass valid_stdlib_types );
  ]

let import_negative_tests =
  [
    ( "unknown member in imported package (value)",
      `Quick,
      fail ~expect:"undefined member 'missing_symbol' in imported package"
        import_unknown_member_value );
    ( "unknown member in imported package (call)",
      `Quick,
      fail ~expect:"undefined member 'missing_fn' in imported package"
        import_unknown_member_call );
    ( "wrong-case callable rejected",
      `Quick,
      fail ~expect:"wrong case" wrong_case_callable );
    ( "wrong-case type rejected",
      `Quick,
      fail ~expect:"wrong case" wrong_case_type );
    ( "stdlib type in expression position rejected",
      `Quick,
      fail ~expect:"is a type, not a value expression" stdlib_type_in_expr );
    ( "stdlib type called as function rejected",
      `Quick,
      fail ~expect:"is a type, not a callable" stdlib_type_called );
  ]

(* ======== Receiver/member interop ======== *)

(* Positive: mux.handle_func with handler function *)
let valid_mux_handle_func =
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

(* Positive: req.form_value *)
let valid_req_form_value =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let name = r.form_value("name");
    println(name);
}
fn main() {
}
|}

(* Positive: req.method *)
let valid_req_method =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let m = r.method;
    println(m);
}
fn main() {
}
|}

(* Positive: w.write_header *)
let valid_write_header =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    w.write_header(200);
}
fn main() {
}
|}

(* Positive: w.write *)
let valid_write =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    w.write("hello");
}
fn main() {
}
|}

(* Positive: combined handler body *)
let valid_combined_handler =
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

(* Negative: Go-cased receiver methods *)
let wrong_case_handle_func =
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

let wrong_case_form_value =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let name = r.FormValue("name");
}
fn main() {
}
|}

let wrong_case_method =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let m = r.Method;
}
fn main() {
}
|}

let wrong_case_write_header =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    w.WriteHeader(200);
}
fn main() {
}
|}

let wrong_case_write =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    w.Write("hello");
}
fn main() {
}
|}

let receiver_member_positive_tests =
  [
    ("mux.handle_func with handler", `Quick, pass valid_mux_handle_func);
    ("req.form_value call", `Quick, pass valid_req_form_value);
    ("req.method field access", `Quick, pass valid_req_method);
    ("w.write_header call", `Quick, pass valid_write_header);
    ("w.write call", `Quick, pass valid_write);
    ( "combined handler body with all receiver members",
      `Quick,
      pass valid_combined_handler );
  ]

let receiver_member_negative_tests =
  [
    ( "Go-cased HandleFunc rejected",
      `Quick,
      fail ~expect:"wrong case" wrong_case_handle_func );
    ( "Go-cased FormValue rejected",
      `Quick,
      fail ~expect:"wrong case" wrong_case_form_value );
    ( "Go-cased Method rejected",
      `Quick,
      fail ~expect:"wrong case" wrong_case_method );
    ( "Go-cased WriteHeader rejected",
      `Quick,
      fail ~expect:"wrong case" wrong_case_write_header );
    ( "Go-cased Write rejected",
      `Quick,
      fail ~expect:"wrong case" wrong_case_write );
  ]

(* ======== Void binding rejection ======== *)

let void_bind_write =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let n = w.write("hello");
}
fn main() {
}
|}

let void_bind_write_header =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    let s = w.write_header(200);
}
fn main() {
}
|}

let void_bind_println = {|
fn main() {
    let x = println("hi");
}
|}

let void_binding_tests =
  [
    ( "let n = w.write(...) rejected",
      `Quick,
      fail ~expect:"cannot bind result of void expression" void_bind_write );
    ( "let s = w.write_header(...) rejected",
      `Quick,
      fail ~expect:"cannot bind result of void expression"
        void_bind_write_header );
    ( "let x = println(...) rejected",
      `Quick,
      fail ~expect:"cannot bind result of void expression" void_bind_println );
  ]

(* ======== Void return rejection ======== *)

let void_return_write =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    return w.write("hello");
}
fn main() {
}
|}

let void_return_write_header =
  {|
use net::http;
fn handler(w: http::ResponseWriter, r: http::Request) {
    return w.write_header(200);
}
fn main() {
}
|}

let void_return_println = {|
fn main() {
    return println("hi");
}
|}

let void_return_tests =
  [
    ( "return w.write(...) rejected",
      `Quick,
      fail ~expect:"cannot return void expression" void_return_write );
    ( "return w.write_header(...) rejected",
      `Quick,
      fail ~expect:"cannot return void expression" void_return_write_header );
    ( "return println(...) rejected",
      `Quick,
      fail ~expect:"cannot return void expression" void_return_println );
  ]

(* ======== Ownership: move / copy / clone ======== *)

(* VAL-OWN-002: Non-Copy assignment moves the source binding *)
let own_assign_move_negative =
  {|
struct Resource {
  pub name: str,
}

fn main() {
  let a = Resource { name: "r1" };
  let b = a;
  println(a.name);
}
|}

(* VAL-OWN-001: Non-Copy values move across consuming call boundaries *)
let own_call_move_negative =
  {|
struct Resource {
  pub name: str,
}

fn consume(r: Resource) {
  println(r.name);
}

fn main() {
  let r = Resource { name: "r1" };
  consume(r);
  println(r.name);
}
|}

(* VAL-OWN-001: Second by-value pass after move *)
let own_call_double_move_negative =
  {|
struct Resource {
  pub name: str,
}

fn consume(r: Resource) {
  println(r.name);
}

fn main() {
  let r = Resource { name: "r1" };
  consume(r);
  consume(r);
}
|}

(* VAL-OWN-003: Copy-eligible values remain usable after assignment *)
let own_copy_assign_positive =
  {|
fn main() {
  let x: i64 = 42;
  let y = x;
  println(x);
  println(y);
}
|}

(* VAL-OWN-003: Copy-eligible values remain usable after by-value call *)
let own_copy_call_positive =
  {|
fn show(n: i64) {
  println(n);
}

fn main() {
  let x: i64 = 42;
  show(x);
  println(x);
}
|}

(* VAL-OWN-003: Bool is Copy-eligible *)
let own_copy_bool_positive =
  {|
fn check(b: bool) {
  println(b);
}

fn main() {
  let b = true;
  check(b);
  println(b);
}
|}

(* VAL-OWN-003: Struct with impl Copy remains usable *)
let own_copy_struct_positive =
  {|
trait Copy {}

struct Point {
  pub x: i64,
  pub y: i64,
}

impl Copy for Point {}

fn show_point(p: Point) {
  println(p.x);
}

fn main() {
  let p = Point { x: 1, y: 2 };
  show_point(p);
  println(p.y);
}
|}

(* Clone escape hatch: clone() on a Clone type prevents move *)
let own_clone_positive =
  {|
trait Clone {
  fn clone(&self) -> Self;
}

struct Resource {
  pub name: str,
}

impl Clone for Resource {
  fn clone(&self) -> Self {
    Resource { name: self.name }
  }
}

fn consume(r: Resource) {
  println(r.name);
}

fn main() {
  let r = Resource { name: "r1" };
  consume(r.clone());
  println(r.name);
}
|}

(* Clone negative: clone() on a non-Clone type is rejected *)
let own_clone_no_impl_negative =
  {|
struct Resource {
  pub name: str,
}

fn main() {
  let r = Resource { name: "r1" };
  let c = r.clone();
  println(c.name);
}
|}

(* VAL-OWN-012: Drop + Copy overlap is rejected *)
let own_drop_copy_overlap_negative =
  {|
trait Drop {
  fn drop(&mut self);
}

trait Copy {}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println(self.name);
  }
}

impl Copy for Resource {}

fn main() {
  let r = Resource { name: "r1" };
  println(r.name);
}
|}

(* VAL-OWN-001: method call on moved value is rejected *)
let own_method_after_move_negative =
  {|
struct Resource {
  pub name: str,
}

impl Resource {
  fn get_name(&self) -> str {
    self.name
  }
}

fn consume(r: Resource) {
  println(r.name);
}

fn main() {
  let r = Resource { name: "r1" };
  consume(r);
  println(r.get_name());
}
|}

(* VAL-OWN-001: field access on moved value is rejected *)
let own_field_after_move_negative =
  {|
struct Resource {
  pub name: str,
}

fn consume(r: Resource) {
  println(r.name);
}

fn main() {
  let r = Resource { name: "r1" };
  consume(r);
  println(r.name);
}
|}

(* VAL-OWN-010: Partial move from field via let binding is rejected *)
let own_partial_move_let_negative =
  {|
struct Inner {
  pub name: str,
}

struct Outer {
  pub inner: Inner,
}

fn main() {
  let o = Outer { inner: Inner { name: "hi" } };
  let x = o.inner;
  println(x.name);
}
|}

(* VAL-OWN-010: Partial move from field via call argument is rejected *)
let own_partial_move_call_negative =
  {|
struct Inner {
  pub name: str,
}

struct Outer {
  pub inner: Inner,
}

fn consume(i: Inner) {
  println(i.name);
}

fn main() {
  let o = Outer { inner: Inner { name: "hi" } };
  consume(o.inner);
}
|}

(* VAL-OWN-010: Partial move from field via assignment is rejected *)
let own_partial_move_assign_negative =
  {|
struct Inner {
  pub name: str,
}

struct Outer {
  pub inner: Inner,
}

fn main() {
  let o = Outer { inner: Inner { name: "hi" } };
  let mut x = Inner { name: "placeholder" };
  x = o.inner;
  println(x.name);
}
|}

(* VAL-OWN-010: Copy field access is still allowed (not a partial move) *)
let own_field_read_copy_positive =
  {|
struct Point {
  pub x: i64,
  pub y: i64,
}

fn show(n: i64) {
  println(n);
}

fn main() {
  let p = Point { x: 1, y: 2 };
  let a = p.x;
  show(p.y);
  println(a);
}
|}

(* VAL-OWN-010: String field access is allowed (not a partial move) *)
let own_field_read_str_positive =
  {|
struct Resource {
  pub name: str,
}

fn main() {
  let r = Resource { name: "hello" };
  let s = r.name;
  println(s);
}
|}

(* VAL-OWN-011: Generic struct move tracking works with concrete instantiation *)
let own_generic_move_negative =
  {|
struct Container<T> {
  pub value: T,
}

impl<T> Container<T> {
  pub fn new(v: T) -> Self {
    Container { value: v }
  }
}

fn consume<T>(c: Container<T>) {
  println("consumed");
}

fn main() {
  let c = Container::new(42);
  consume(c);
  consume(c);
}
|}

(* VAL-OWN-011: Generic struct with Copy stays usable *)
let own_generic_copy_positive =
  {|
trait Copy {}

struct Wrapper<T> {
  pub value: T,
}

impl<T> Copy for Wrapper<T> {}

impl<T> Wrapper<T> {
  pub fn new(v: T) -> Self {
    Wrapper { value: v }
  }
}

fn use_wrapper<T>(w: Wrapper<T>) {
  println("used");
}

fn main() {
  let w = Wrapper::new(42);
  use_wrapper(w);
  use_wrapper(w);
}
|}

(* VAL-OWN-011: Generic enum variant constructor infers type params *)
let own_generic_enum_constructor_positive =
  {|
enum Wrapper<T> {
  Some(T),
  None,
}

fn use_wrapper(w: Wrapper<i64>) {
  println("used");
}

fn main() {
  let w = Wrapper::Some(42);
  use_wrapper(w);
}
|}

(* VAL-OWN-011: Generic enum variant constructor with Copy stays usable *)
let own_generic_enum_copy_positive =
  {|
trait Copy {}

enum Wrapper<T> {
  Some(T),
  None,
}

impl<T> Copy for Wrapper<T> {}

fn use_wrapper<T>(w: Wrapper<T>) {
  println("used");
}

fn main() {
  let w = Wrapper::Some(42);
  use_wrapper(w);
  use_wrapper(w);
}
|}

(* VAL-OWN-011: Generic enum variant constructor move is rejected *)
let own_generic_enum_move_negative =
  {|
enum Wrapper<T> {
  Some(T),
  None,
}

fn consume<T>(w: Wrapper<T>) {
  println("consumed");
}

fn main() {
  let w = Wrapper::Some(42);
  consume(w);
  consume(w);
}
|}

(* VAL-OWN-011: Generic enum Clone with Self in method signature *)
let own_generic_enum_clone_positive =
  {|
trait Clone {
  fn clone(&self) -> Self;
}

enum Wrapper<T> {
  Some(T),
  None,
}

impl<T> Clone for Wrapper<T> {
  fn clone(&self) -> Self {
    match self {
      Wrapper::Some(v) => Wrapper::Some(v),
      Wrapper::None => Wrapper::None,
    }
  }
}

fn consume(w: Wrapper<i64>) {
  println("consumed");
}

fn main() {
  let w = Wrapper::Some(42);
  consume(w.clone());
  println("original-alive");
}
|}

(* Negative: mismatched generic enum payload type *)
let own_generic_enum_payload_mismatch_negative =
  {|
enum Wrapper<T> {
  Some(T),
  None,
}

fn take_wrapper(w: Wrapper<i64>) {
  println("ok");
}

fn main() {
  let w = Wrapper::Some("hello");
  take_wrapper(w);
}
|}

(* VAL-OWN-011: Repeated type param with conflicting types is rejected *)
let own_generic_enum_repeated_tparam_conflict_negative =
  {|
enum Pair<T> {
  Both(T, T),
}

fn main() {
  let p = Pair::Both(42, "hello");
}
|}

(* VAL-OWN-011: Repeated type param with compatible types is accepted *)
let own_generic_enum_repeated_tparam_positive =
  {|
enum Pair<T> {
  Both(T, T),
}

fn use_pair(p: Pair<i64>) {
  println("used");
}

fn main() {
  let p = Pair::Both(42, 99);
  use_pair(p);
}
|}

(* VAL-OWN-011: Nested generic enum payload preserves instantiation *)
let own_generic_enum_nested_payload_positive =
  {|
enum Outer<T> {
  Wrapped(Option<T>),
}

fn use_outer(o: Outer<i64>) {
  println("used");
}

fn main() {
  let o = Outer::Wrapped(Some(42));
  use_outer(o);
}
|}

(* VAL-OWN-001: Consuming self method moves non-Copy receiver *)
let own_consuming_self_move_negative =
  {|
struct Resource {
  pub name: str,
}

impl Resource {
  fn consume(self) {
    println(self.name);
  }
}

fn main() {
  let r = Resource { name: "r1" };
  r.consume();
  println(r.name);
}
|}

(* VAL-OWN-001: Non-consuming &self keeps receiver usable *)
let own_ref_self_positive =
  {|
struct Resource {
  pub name: str,
}

impl Resource {
  fn get_name(&self) -> str {
    self.name
  }
}

fn main() {
  let r = Resource { name: "r1" };
  println(r.get_name());
  println(r.get_name());
}
|}

(* VAL-OWN-001: Non-consuming &mut self keeps receiver usable *)
let own_mutref_self_positive =
  {|
struct Counter {
  pub value: i64,
}

impl Counter {
  fn increment(&mut self) {
    self.value = self.value + 1;
  }

  fn get(&self) -> i64 {
    self.value
  }
}

fn main() {
  let mut c = Counter { value: 0 };
  c.increment();
  c.increment();
  println(c.get());
}
|}

(* VAL-OWN-001: Consuming self on Copy type keeps receiver usable *)
let own_consuming_self_copy_positive =
  {|
trait Copy {}

struct Point {
  pub x: i64,
  pub y: i64,
}

impl Copy for Point {}

impl Point {
  fn consume(self) -> i64 {
    self.x + self.y
  }
}

fn main() {
  let p = Point { x: 1, y: 2 };
  println(p.consume());
  println(p.consume());
}
|}

(* VAL-OWN-001: Double consuming self call rejected *)
let own_consuming_self_double_negative =
  {|
struct Resource {
  pub name: str,
}

impl Resource {
  fn consume(self) {
    println(self.name);
  }
}

fn main() {
  let r = Resource { name: "r1" };
  r.consume();
  r.consume();
}
|}

(* VAL-OWN-010: Enum-payload pattern destructuring partial move rejected *)
let own_enum_pattern_partial_move_negative =
  {|
struct Inner {
  pub name: str,
}

enum Wrapper {
  Some(Inner),
  None,
}

fn main() {
  let w = Wrapper::Some(Inner { name: "hi" });
  match w {
    Wrapper::Some(inner) => println(inner.name),
    Wrapper::None => println("none"),
  }
}
|}

(* VAL-OWN-010: Enum wildcard pattern is fine (no extraction) *)
let own_enum_pattern_wildcard_positive =
  {|
enum Shape {
  Circle(f64),
  Rect(str, str),
}

fn main() {
  let s = Shape::Rect("wide", "tall");
  match s {
    Shape::Circle(_) => println("circle"),
    Shape::Rect(_, _) => println("rect"),
  }
}
|}

(* VAL-OWN-010: Copy enum pattern destructuring is fine *)
let own_enum_pattern_copy_positive =
  {|
trait Copy {}

enum Pair {
  Two(i64, i64),
  None,
}

impl Copy for Pair {}

fn main() {
  let p = Pair::Two(1, 2);
  match p {
    Pair::Two(a, b) => println(a + b),
    Pair::None => println(0),
  }
}
|}

(* VAL-OWN-010: Enum struct-pattern destructuring partial move rejected *)
let own_enum_struct_pattern_partial_move_negative =
  {|
struct Inner {
  pub name: str,
}

enum Wrapper {
  Some { inner: Inner },
  None,
}

fn main() {
  let w = Wrapper::Some { inner: Inner { name: "hi" } };
  match w {
    Wrapper::Some { inner } => println(inner.name),
    Wrapper::None => println("none"),
  }
}
|}

(* VAL-OWN-001: Trait-bound dispatch - non-consuming &self keeps receiver usable *)
let own_trait_dispatch_ref_self_positive =
  {|
trait Inspect {
  fn show(&self);
}

struct Resource {
  pub name: str,
}

impl Inspect for Resource {
  fn show(&self) {
    println(self.name);
  }
}

fn use_inspector<T: Inspect>(t: T) {
  t.show();
  t.show();
}

fn main() {
  let r = Resource { name: "hello" };
  use_inspector(r);
}
|}

(* VAL-OWN-010: Option with Copy payload pattern is fine *)
let own_option_copy_pattern_positive =
  {|
fn main() {
  let opt: Option<i64> = Some(42);
  match opt {
    Option::Some(n) => println(n),
    Option::None => println(0),
  }
}
|}

(* VAL-OWN-010: Result with Copy payload pattern is fine *)
let own_result_copy_pattern_positive =
  {|
fn main() {
  let res: Result<i64, str> = Ok(42);
  match res {
    Result::Ok(n) => println(n),
    Result::Err(e) => println(e),
  }
}
|}

(* VAL-OWN-010: Option/Result wildcard pattern is fine *)
let own_option_wildcard_pattern_positive =
  {|
struct Inner {
  pub name: str,
}

fn main() {
  let opt: Option<Inner> = Some(Inner { name: "hi" });
  match opt {
    Option::Some(_) => println("some"),
    Option::None => println("none"),
  }
}
|}

(* VAL-OWN-001: Trait-bound dispatch consumes self - later use rejected *)
let own_trait_dispatch_consume_negative =
  {|
trait Consume {
  fn take(self);
}

struct Resource {
  pub name: str,
}

impl Consume for Resource {
  fn take(self) {
    println(self.name);
  }
}

fn use_consumer<T: Consume>(t: T) {
  t.take();
  t.take();
}
|}

(* VAL-OWN-010: Option with non-Copy payload partial move rejected *)
let own_option_pattern_partial_move_negative =
  {|
struct Inner {
  pub name: str,
}

fn main() {
  let opt: Option<Inner> = Some(Inner { name: "hi" });
  match opt {
    Option::Some(inner) => println(inner.name),
    Option::None => println("none"),
  }
}
|}

(* VAL-OWN-010: Result with non-Copy payload partial move rejected *)
let own_result_pattern_partial_move_negative =
  {|
struct Inner {
  pub name: str,
}

fn main() {
  let res: Result<Inner, str> = Ok(Inner { name: "hi" });
  match res {
    Result::Ok(inner) => println(inner.name),
    Result::Err(e) => println(e),
  }
}
|}

(* VAL-OWN-010: Nested Option with non-Copy payload partial move rejected *)
let own_nested_option_pattern_partial_move_negative =
  {|
struct Inner {
  pub name: str,
}

fn main() {
  let opt: Option<Option<Inner>> = Some(Some(Inner { name: "hi" }));
  match opt {
    Option::Some(Option::Some(inner)) => println(inner.name),
    Option::Some(Option::None) => println("none"),
    Option::None => println("none"),
  }
}
|}

(* VAL-OWN-010: Nested Result with non-Copy payload partial move rejected *)
let own_nested_result_pattern_partial_move_negative =
  {|
struct Inner {
  pub name: str,
}

fn main() {
  let res: Result<Option<Inner>, str> = Ok(Some(Inner { name: "hi" }));
  match res {
    Result::Ok(Option::Some(inner)) => println(inner.name),
    Result::Ok(Option::None) => println("none"),
    Result::Err(e) => println(e),
  }
}
|}

(* VAL-OWN-010: Nested Option in Result with non-Copy payload rejected *)
let own_option_in_result_pattern_partial_move_negative =
  {|
struct Inner {
  pub name: str,
}

fn main() {
  let res: Result<Result<Inner, str>, str> = Ok(Ok(Inner { name: "hi" }));
  match res {
    Result::Ok(Result::Ok(inner)) => println(inner.name),
    Result::Ok(Result::Err(e)) => println(e),
    Result::Err(e) => println(e),
  }
}
|}

(* VAL-OWN-010: Consuming self match with enum payload partial move rejected *)
let own_self_match_enum_payload_negative =
  {|
struct Inner {
  pub name: str,
}

enum Wrapper {
  Some(Inner),
  None,
}

impl Wrapper {
  fn inspect(self) {
    match self {
      Wrapper::Some(inner) => println(inner.name),
      Wrapper::None => println("none"),
    }
  }
}
|}

let ownership_positive_tests =
  [
    ("copy i64 survives assignment", `Quick, pass own_copy_assign_positive);
    ("copy i64 survives call", `Quick, pass own_copy_call_positive);
    ("copy bool survives call", `Quick, pass own_copy_bool_positive);
    ("copy struct survives call", `Quick, pass own_copy_struct_positive);
    ("clone escape hatch", `Quick, pass own_clone_positive);
    ("copy field access allowed", `Quick, pass own_field_read_copy_positive);
    ("string field access allowed", `Quick, pass own_field_read_str_positive);
    ("generic Copy struct reusable", `Quick, pass own_generic_copy_positive);
    ( "generic enum constructor infers type",
      `Quick,
      pass own_generic_enum_constructor_positive );
    ("generic enum Copy reusable", `Quick, pass own_generic_enum_copy_positive);
    ( "generic enum Clone with Self lowering",
      `Quick,
      pass own_generic_enum_clone_positive );
    ( "generic enum repeated tparam compatible",
      `Quick,
      pass own_generic_enum_repeated_tparam_positive );
    ( "generic enum nested payload preserves instantiation",
      `Quick,
      pass own_generic_enum_nested_payload_positive );
    ( "non-consuming &self keeps receiver usable",
      `Quick,
      pass own_ref_self_positive );
    ( "non-consuming &mut self keeps receiver usable",
      `Quick,
      pass own_mutref_self_positive );
    ( "consuming self on Copy type keeps receiver usable",
      `Quick,
      pass own_consuming_self_copy_positive );
    ( "enum wildcard pattern is fine",
      `Quick,
      pass own_enum_pattern_wildcard_positive );
    ( "Copy enum pattern destructuring is fine",
      `Quick,
      pass own_enum_pattern_copy_positive );
    ( "trait-bound &self dispatch keeps receiver usable",
      `Quick,
      pass own_trait_dispatch_ref_self_positive );
    ( "Option with Copy payload pattern is fine",
      `Quick,
      pass own_option_copy_pattern_positive );
    ( "Result with Copy payload pattern is fine",
      `Quick,
      pass own_result_copy_pattern_positive );
    ( "Option with non-Copy payload wildcard is fine",
      `Quick,
      pass own_option_wildcard_pattern_positive );
  ]

let ownership_negative_tests =
  [
    ( "non-Copy assign moves source",
      `Quick,
      fail ~expect:"use of moved value" own_assign_move_negative );
    ( "non-Copy call moves argument",
      `Quick,
      fail ~expect:"use of moved value" own_call_move_negative );
    ( "non-Copy double call moves",
      `Quick,
      fail ~expect:"use of moved value" own_call_double_move_negative );
    ( "clone on non-Clone type rejected",
      `Quick,
      fail ~expect:"does not implement Clone" own_clone_no_impl_negative );
    ( "Drop + Copy overlap rejected",
      `Quick,
      fail ~expect:"cannot implement both Drop and Copy"
        own_drop_copy_overlap_negative );
    ( "method call after move rejected",
      `Quick,
      fail ~expect:"use of moved value" own_method_after_move_negative );
    ( "field access after move rejected",
      `Quick,
      fail ~expect:"use of moved value" own_field_after_move_negative );
    ( "partial move from field via let rejected",
      `Quick,
      fail ~expect:"partial moves are not supported"
        own_partial_move_let_negative );
    ( "partial move from field via call rejected",
      `Quick,
      fail ~expect:"partial moves are not supported"
        own_partial_move_call_negative );
    ( "partial move from field via assign rejected",
      `Quick,
      fail ~expect:"partial moves are not supported"
        own_partial_move_assign_negative );
    ( "generic struct move double-use rejected",
      `Quick,
      fail ~expect:"use of moved value" own_generic_move_negative );
    ( "generic enum variant move double-use rejected",
      `Quick,
      fail ~expect:"use of moved value" own_generic_enum_move_negative );
    ( "generic enum payload mismatch rejected",
      `Quick,
      fail ~expect:"type mismatch" own_generic_enum_payload_mismatch_negative );
    ( "generic enum repeated tparam conflict rejected",
      `Quick,
      fail ~expect:"conflicting types for type parameter"
        own_generic_enum_repeated_tparam_conflict_negative );
    ( "consuming self moves non-Copy receiver",
      `Quick,
      fail ~expect:"use of moved value" own_consuming_self_move_negative );
    ( "double consuming self call rejected",
      `Quick,
      fail ~expect:"use of moved value" own_consuming_self_double_negative );
    ( "enum-payload pattern partial move rejected",
      `Quick,
      fail ~expect:"partial moves are not supported"
        own_enum_pattern_partial_move_negative );
    ( "enum struct-pattern partial move rejected",
      `Quick,
      fail ~expect:"partial moves are not supported"
        own_enum_struct_pattern_partial_move_negative );
    ( "trait-bound consuming self dispatch rejected",
      `Quick,
      fail ~expect:"use of moved value" own_trait_dispatch_consume_negative );
    ( "Option non-Copy payload pattern rejected",
      `Quick,
      fail ~expect:"partial moves are not supported"
        own_option_pattern_partial_move_negative );
    ( "Result non-Copy payload pattern rejected",
      `Quick,
      fail ~expect:"partial moves are not supported"
        own_result_pattern_partial_move_negative );
    ( "nested Option non-Copy payload pattern rejected",
      `Quick,
      fail ~expect:"partial moves are not supported"
        own_nested_option_pattern_partial_move_negative );
    ( "nested Result non-Copy payload pattern rejected",
      `Quick,
      fail ~expect:"partial moves are not supported"
        own_nested_result_pattern_partial_move_negative );
    ( "nested Option in Result non-Copy payload pattern rejected",
      `Quick,
      fail ~expect:"partial moves are not supported"
        own_option_in_result_pattern_partial_move_negative );
    ( "consuming self match enum payload rejected",
      `Quick,
      fail ~expect:"partial moves are not supported"
        own_self_match_enum_payload_negative );
  ]

let () =
  Alcotest.run "typecheck"
    [
      ("positive", positive_tests);
      ("negative", negative_tests);
      ("import-positive", import_positive_tests);
      ("import-negative", import_negative_tests);
      ("receiver-member-positive", receiver_member_positive_tests);
      ("receiver-member-negative", receiver_member_negative_tests);
      ("void-binding", void_binding_tests);
      ("void-return", void_return_tests);
      ("ownership-positive", ownership_positive_tests);
      ("ownership-negative", ownership_negative_tests);
    ]
