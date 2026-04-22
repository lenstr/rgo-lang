(* Ownership codegen tests: move/copy/clone/drop behavior and cleanup scheduling.
   Extracted from test_codegen.ml to keep ownership test coverage focused. *)

open Codegen_test_helpers

(* ======== Ownership positive tests ======== *)

(* VAL-OWN-003: Copy i64 survives assignment and by-value call *)
let test_own_copy_i64_runtime () =
  let src =
    {|
fn show(n: i64) {
  println(n);
}

fn main() {
  let x: i64 = 42;
  let y = x;
  show(x);
  println(y);
}
|}
  in
  compile_and_check ~expected_output:"42\n42\n" src |> ignore

(* VAL-OWN-003: Copy bool survives call *)
let test_own_copy_bool_runtime () =
  let src =
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
  in
  compile_and_check ~expected_output:"true\ntrue\n" src |> ignore

(* VAL-OWN-003: Copy struct with impl Copy survives call *)
let test_own_copy_struct_runtime () =
  let src =
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
  in
  compile_and_check ~expected_output:"1\n2\n" src |> ignore

(* VAL-OWN-004: clone() escape hatch - runtime output proves both original
   and clone remain independently usable *)
let test_own_clone_runtime () =
  let src =
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
  println("consumed: " + r.name);
}

fn main() {
  let r = Resource { name: "original" };
  consume(r.clone());
  println("still here: " + r.name);
}
|}
  in
  compile_and_check
    ~expected_output:"consumed: original\nstill here: original\n" src
  |> ignore

(* ======== Ownership negative tests ======== *)

(* VAL-OWN-002: Non-Copy assignment moves source u2014 negative *)
let test_own_assign_move_negative () =
  compile_expect_error ~expect:"use of moved value"
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
    ()

(* VAL-OWN-001: Non-Copy call moves argument u2014 negative *)
let test_own_call_move_negative () =
  compile_expect_error ~expect:"use of moved value"
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
    ()

(* VAL-OWN-012: Drop + Copy overlap rejected u2014 negative *)
let test_own_drop_copy_overlap () =
  compile_expect_error ~expect:"cannot implement both Drop and Copy"
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
    ()

(* Clone on non-Clone type rejected u2014 negative *)
let test_own_clone_no_impl () =
  compile_expect_error ~expect:"does not implement Clone"
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
    ()

(* ======== Ownership cleanup tests (VAL-OWN-005 through VAL-OWN-009) ======== *)

(* VAL-OWN-005: Drop cleanup remains exactly-once on normal scope exit,
   reverse-declaration order *)
let test_own_cleanup_normal_exit () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn main() {
  let a = Resource { name: "first" };
  let b = Resource { name: "second" };
  let c = Resource { name: "third" };
  println("alive:" + a.name);
  println("alive:" + b.name);
  println("alive:" + c.name);
}
|}
  in
  compile_and_check
    ~expected_output:
      "alive:first\n\
       alive:second\n\
       alive:third\n\
       drop:third\n\
       drop:second\n\
       drop:first\n"
    src
  |> ignore

(* VAL-OWN-006: Drop cleanup remains exactly-once on early return *)
let test_own_cleanup_early_return () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn early() -> i64 {
  let a = Resource { name: "first" };
  let b = Resource { name: "second" };
  println("before-return");
  return 42;
}

fn main() {
  let result = early();
  println(result);
}
|}
  in
  compile_and_check
    ~expected_output:"before-return\ndrop:second\ndrop:first\n42\n" src
  |> ignore

(* VAL-OWN-006: Drop cleanup remains exactly-once on nested ? exit *)
let test_own_cleanup_question_exit () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn might_fail(ok: bool) -> Result<i64, str> {
  if ok {
    Ok(1)
  } else {
    Err("fail")
  }
}

fn try_work() -> Result<i64, str> {
  let a = Resource { name: "r1" };
  let val = might_fail(false)?;
  println("unreachable");
  Ok(val)
}

fn main() {
  let result = try_work();
  println("done");
}
|}
  in
  compile_and_check ~expected_output:"drop:r1\ndone\n" src |> ignore

(* VAL-OWN-007: Overwrite cleanup runs exactly once for mutable rebinding *)
let test_own_cleanup_overwrite () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn main() {
  let mut r = Resource { name: "old" };
  println("before-overwrite");
  r = Resource { name: "new" };
  println("after-overwrite");
}
|}
  in
  compile_and_check
    ~expected_output:"before-overwrite\ndrop:old\nafter-overwrite\ndrop:new\n"
    src
  |> ignore

(* VAL-OWN-008: By-value returns transfer ownership without double cleanup *)
let test_own_cleanup_byval_return () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn make() -> Resource {
  let r = Resource { name: "transferred" };
  r
}

fn main() {
  let owned = make();
  println("caller:" + owned.name);
}
|}
  in
  compile_and_check ~expected_output:"caller:transferred\ndrop:transferred\n"
    src
  |> ignore

(* VAL-OWN-009: By-value parameters are cleaned at callee exit when
   ownership is retained there *)
let test_own_cleanup_callee_param () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn consume(r: Resource) {
  println("inside:" + r.name);
}

fn main() {
  let r = Resource { name: "param" };
  consume(r);
  println("after-call");
}
|}
  in
  compile_and_check ~expected_output:"inside:param\ndrop:param\nafter-call\n"
    src
  |> ignore

(* ======== Non-consuming call guard suppression tests ======== *)

(* Passing a Drop-managed binding to println does not suppress cleanup *)
let test_own_non_consuming_println_cleanup () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn main() {
  let a = Resource { name: "alpha" };
  println(a.name);
  println("still-alive");
}
|}
  in
  compile_and_check ~expected_output:"alpha\nstill-alive\ndrop:alpha\n" src
  |> ignore

(* println with the binding itself (not field) still keeps cleanup *)
let test_own_non_consuming_println_binding () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

trait Display {
  fn to_str(&self) -> str;
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

impl Display for Resource {
  fn to_str(&self) -> str {
    self.name
  }
}

fn main() {
  let a = Resource { name: "alpha" };
  println(a.to_str());
  println("still-alive");
}
|}
  in
  compile_and_check ~expected_output:"alpha\nstill-alive\ndrop:alpha\n" src
  |> ignore

(* Consuming call still suppresses cleanup guard correctly *)
let test_own_consuming_call_still_suppresses () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn consume(r: Resource) {
  println("consumed:" + r.name);
}

fn main() {
  let a = Resource { name: "alpha" };
  consume(a);
  println("after-consume");
}
|}
  in
  compile_and_check
    ~expected_output:"consumed:alpha\ndrop:alpha\nafter-consume\n" src
  |> ignore

(* Mixed consuming and non-consuming calls: cleanup fires exactly once *)
let test_own_mixed_consuming_non_consuming () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn consume(r: Resource) {
  println("consumed:" + r.name);
}

fn main() {
  let a = Resource { name: "kept" };
  let b = Resource { name: "given" };
  println(a.name);
  consume(b);
  println(a.name);
  println("end");
}
|}
  in
  compile_and_check
    ~expected_output:"kept\nconsumed:given\ndrop:given\nkept\nend\ndrop:kept\n"
    src
  |> ignore

(* ======== Partial move tests (VAL-OWN-010) ======== *)

(* VAL-OWN-010: Partial move from field via let binding u2014 negative *)
let test_own_partial_move_let_negative () =
  compile_expect_error ~expect:"partial moves are not supported"
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
    ()

(* VAL-OWN-010: Partial move from field via call argument u2014 negative *)
let test_own_partial_move_call_negative () =
  compile_expect_error ~expect:"partial moves are not supported"
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
    ()

(* ======== Lexical scope cleanup tests ======== *)

(* Drop binding in a nested block is cleaned at block exit, not function exit *)
let test_own_nested_block_cleanup () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn main() {
  println("before-block");
  {
    let a = Resource { name: "inner" };
    println("inside-block");
  }
  println("after-block");
}
|}
  in
  compile_and_check
    ~expected_output:"before-block\ninside-block\ndrop:inner\nafter-block\n" src
  |> ignore

(* Drop binding in a loop body is cleaned each iteration *)
let test_own_loop_cleanup () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn main() {
  let mut i = 0;
  while i < 2 {
    let a = Resource { name: "loop" };
    println("iter");
    i = i + 1;
  }
  println("done");
}
|}
  in
  compile_and_check ~expected_output:"iter\ndrop:loop\niter\ndrop:loop\ndone\n"
    src
  |> ignore

(* Consuming move in let init suppresses guard exactly once *)
let test_own_let_init_consume_suppresses () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn take(r: Resource) -> Resource {
  r
}

fn main() {
  let a = Resource { name: "original" };
  let b = take(a);
  println("moved");
}
|}
  in
  compile_and_check ~expected_output:"moved\ndrop:original\n" src |> ignore

(* Consuming move in return expression suppresses guard exactly once *)
let test_own_return_consume_suppresses () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn take(r: Resource) -> Resource {
  r
}

fn wrap() -> Resource {
  let a = Resource { name: "wrapped" };
  take(a)
}

fn main() {
  let r = wrap();
  println("caller:" + r.name);
}
|}
  in
  compile_and_check ~expected_output:"caller:wrapped\ndrop:wrapped\n" src
  |> ignore

(* Imported stdlib non-consuming call does not suppress Drop cleanup *)
let test_own_stdlib_call_no_suppress () =
  let src =
    {|
use net::http;

trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn main() {
  let a = Resource { name: "alpha" };
  let _mux = http::new_serve_mux();
  println(a.name);
  println("still-alive");
}
|}
  in
  compile_and_check ~expected_output:"alpha\nstill-alive\ndrop:alpha\n" src
  |> ignore

(* ======== Generic ownership path tests (VAL-OWN-011) ======== *)

(* VAL-OWN-011: Generic struct with Drop u2014 cleanup preserves concrete
   instantiation and fires exactly once *)
let test_own_generic_drop_cleanup () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Container<T> {
  pub value: T,
}

impl<T> Container<T> {
  pub fn new(v: T) -> Self {
    Container { value: v }
  }
}

impl<T> Drop for Container<T> {
  fn drop(&mut self) {
    println("drop-container");
  }
}

fn main() {
  let c = Container::new(42);
  println("alive");
}
|}
  in
  let go = compile_and_check ~expected_output:"alive\ndrop-container\n" src in
  Alcotest.(check bool)
    "generic struct type" true
    (contains go "type Container[T any] struct");
  Alcotest.(check bool)
    "generic Drop method" true
    (contains go "func (self *Container[T]) Drop()")

(* VAL-OWN-011: Generic struct move tracking u2014 moved value has cleanup
   transferred to destination *)
let test_own_generic_move_cleanup () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Container<T> {
  pub value: T,
}

impl<T> Container<T> {
  pub fn new(v: T) -> Self {
    Container { value: v }
  }
}

impl<T> Drop for Container<T> {
  fn drop(&mut self) {
    println("drop-container");
  }
}

fn main() {
  let a = Container::new("hello");
  let b = a;
  println("done");
}
|}
  in
  compile_and_check ~expected_output:"done\ndrop-container\n" src |> ignore

(* VAL-OWN-011: Generic struct with Copy u2014 copies keep original alive *)
let test_own_generic_copy_runtime () =
  let src =
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

  pub fn get(&self) -> T {
    self.value
  }
}

fn use_wrapper<T>(w: Wrapper<T>) {
  println("used");
}

fn main() {
  let w = Wrapper::new(42);
  use_wrapper(w);
  use_wrapper(w);
  println("still-alive");
}
|}
  in
  compile_and_check ~expected_output:"used\nused\nstill-alive\n" src |> ignore

(* VAL-OWN-011: Two distinct generic instantiations have independent
   cleanup scheduling *)
let test_own_generic_two_instantiations () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Box<T> {
  pub value: T,
}

impl<T> Box<T> {
  pub fn new(v: T) -> Self {
    Box { value: v }
  }
}

impl<T> Drop for Box<T> {
  fn drop(&mut self) {
    println("drop-box");
  }
}

fn main() {
  let a = Box::new(42);
  let b = Box::new("hello");
  println("alive");
}
|}
  in
  compile_and_check ~expected_output:"alive\ndrop-box\ndrop-box\n" src |> ignore

(* VAL-OWN-011: Generic clone escape hatch at boundary *)
let test_own_generic_clone_runtime () =
  let src =
    {|
trait Clone {
  fn clone(&self) -> Self;
}

struct Container<T> {
  pub value: T,
}

impl<T> Container<T> {
  pub fn new(v: T) -> Self {
    Container { value: v }
  }
}

impl<T> Clone for Container<T> {
  fn clone(&self) -> Self {
    Container { value: self.value }
  }
}

fn consume<T>(c: Container<T>) {
  println("consumed");
}

fn main() {
  let c = Container::new(42);
  consume(c.clone());
  println("still-alive");
}
|}
  in
  compile_and_check ~expected_output:"consumed\nstill-alive\n" src |> ignore

(* VAL-OWN-011: Generic enum variant constructor with Drop u2014 direct
   construction preserves concrete instantiation and cleanup fires once *)
let test_own_generic_enum_drop () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

enum Wrapper<T> {
  Some(T),
  None,
}

impl<T> Drop for Wrapper<T> {
  fn drop(&mut self) {
    println("drop-wrapper");
  }
}

fn main() {
  let w = Wrapper::Some(42);
  println("alive");
}
|}
  in
  let go = compile_and_check ~expected_output:"alive\ndrop-wrapper\n" src in
  Alcotest.(check bool)
    "generic enum variant type" true
    (contains go "WrapperSome[int64]")

(* VAL-OWN-011: Generic enum variant constructor with move u2014 moved value
   has cleanup transferred to destination *)
let test_own_generic_enum_move () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

enum Wrapper<T> {
  Some(T),
  None,
}

impl<T> Drop for Wrapper<T> {
  fn drop(&mut self) {
    println("drop-wrapper");
  }
}

fn main() {
  let a = Wrapper::Some("hello");
  let b = a;
  println("done");
}
|}
  in
  compile_and_check ~expected_output:"done\ndrop-wrapper\n" src |> ignore

(* VAL-OWN-011: Generic enum variant constructor with Copy u2014 copies keep
   original alive *)
let test_own_generic_enum_copy () =
  let src =
    {|
trait Copy {}

enum Wrapper<T> {
  Some(T),
  None,
}

impl<T> Copy for Wrapper<T> {}

fn main() {
  let w = Wrapper::Some(42);
  let w2 = w;
  let w3 = w;
  println("still-alive");
}
|}
  in
  compile_and_check ~expected_output:"still-alive\n" src |> ignore

(* VAL-OWN-011: Generic enum variant constructor with Clone u2014 explicit
   clone at boundary *)
(* VAL-OWN-011: Two distinct generic enum instantiations have independent
   cleanup scheduling *)
let test_own_generic_enum_clone () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

enum Wrapper<T> {
  Some(T),
  None,
}

impl<T> Drop for Wrapper<T> {
  fn drop(&mut self) {
    println("drop-wrapper");
  }
}

fn main() {
  let a = Wrapper::Some(42);
  let b = Wrapper::Some("hello");
  println("alive");
}
|}
  in
  compile_and_check ~expected_output:"alive\ndrop-wrapper\ndrop-wrapper\n" src
  |> ignore

(* VAL-OWN-011: Generic enum Clone with Self lowering u2014 clone at boundary
   proves original remains usable while clone is independently consumed *)
let test_own_generic_enum_clone_runtime () =
  let src =
    {|
trait Clone {
  fn clone(&self) -> Self;
}

trait Drop {
  fn drop(&mut self);
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

impl<T> Drop for Wrapper<T> {
  fn drop(&mut self) {
    println("drop-wrapper");
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
  in
  compile_and_check
    ~expected_output:"consumed\ndrop-wrapper\noriginal-alive\ndrop-wrapper\n"
    src
  |> ignore

(* VAL-OWN-011: Generic enum clone example fixture builds and runs *)
let test_generic_enum_clone_example () =
  let src = read_file "../examples/generic_enum_clone.rg" in
  let _go =
    compile_and_check
      ~expected_output:"consumed\ndrop-wrapper\noriginal-alive\ndrop-wrapper\n"
      src
  in
  ()

(* Negative: mismatched generic enum payload type is rejected *)
let test_own_generic_enum_mismatch_negative () =
  compile_expect_error ~expect:"type mismatch"
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
    ()

(* VAL-OWN-011: Nested generic enum payload preserves concrete instantiation
   in generated Go u2014 Option<i64> payload keeps int64 type arg *)
let test_own_generic_enum_nested_payload () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

enum Outer<T> {
  Wrapped(Option<T>),
  Empty,
}

impl<T> Drop for Outer<T> {
  fn drop(&mut self) {
    println("drop-outer");
  }
}

fn main() {
  let o = Outer::Wrapped(Some(42));
  println("alive");
}
|}
  in
  let go = compile_and_check ~expected_output:"alive\ndrop-outer\n" src in
  Alcotest.(check bool)
    "nested generic enum variant type" true
    (contains go "OuterWrapped[int64]")

(* VAL-OWN-011: Repeated type param with conflicting types rejected *)
let test_own_generic_enum_repeated_conflict () =
  compile_expect_error ~expect:"conflicting types for type parameter"
    {|
enum Pair<T> {
  Both(T, T),
}

fn main() {
  let p = Pair::Both(42, "hello");
}
|}
    ()

(* VAL-OWN-011: Nested generic enum payload with move u2014 ownership transfers *)
let test_own_generic_enum_nested_move () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

enum Outer<T> {
  Wrapped(Option<T>),
  Empty,
}

impl<T> Drop for Outer<T> {
  fn drop(&mut self) {
    println("drop-outer");
  }
}

fn main() {
  let a = Outer::Wrapped(Some(42));
  let b = a;
  println("done");
}
|}
  in
  compile_and_check ~expected_output:"done\ndrop-outer\n" src |> ignore

(* VAL-OWN-011: generic_enum_nested.rg example fixture builds and runs *)
let test_generic_enum_nested_example () =
  let src = read_file "../examples/generic_enum_nested.rg" in
  let go =
    compile_and_check
      ~expected_output:
        "a-alive\n\
         b-moved\n\
         d-copied\n\
         pair-alive\n\
         drop-pair\n\
         drop-outer\n\
         drop-outer\n"
      src
  in
  Alcotest.(check bool)
    "nested payload preserves type" true
    (contains go "OuterWrapped[int64]");
  Alcotest.(check bool)
    "repeated tparam preserves type" true
    (contains go "PairBoth[int64]")

(* ======== Specialized return fast-path cleanup tests ======== *)

(* VAL-OWN-006: return Ok(...) cleans up nested Drop bindings exactly once *)
let test_own_return_ok_nested_cleanup () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn early_ok() -> Result<i64, str> {
  let outer = Resource { name: "outer" };
  {
    let inner = Resource { name: "inner" };
    println("before-ok");
    return Ok(42);
  }
}

fn main() {
  let result = early_ok();
  println("done");
}
|}
  in
  compile_and_check ~expected_output:"before-ok\ndrop:inner\ndrop:outer\ndone\n"
    src
  |> ignore

(* VAL-OWN-006: return Err(...) cleans up nested Drop bindings exactly once *)
let test_own_return_err_nested_cleanup () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn early_err() -> Result<i64, str> {
  let outer = Resource { name: "outer" };
  {
    let inner = Resource { name: "inner" };
    println("before-err");
    return Err("fail");
  }
}

fn main() {
  let result = early_err();
  println("done");
}
|}
  in
  compile_and_check
    ~expected_output:"before-err\ndrop:inner\ndrop:outer\ndone\n" src
  |> ignore

(* VAL-OWN-006: return Some(...) cleans up nested Drop bindings exactly once *)
let test_own_return_some_nested_cleanup () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn early_some() -> Option<i64> {
  let outer = Resource { name: "outer" };
  {
    let inner = Resource { name: "inner" };
    println("before-some");
    return Some(42);
  }
}

fn main() {
  let r = early_some();
  println("done");
}
|}
  in
  compile_and_check
    ~expected_output:"before-some\ndrop:inner\ndrop:outer\ndone\n" src
  |> ignore

(* VAL-OWN-006: return None cleans up nested Drop bindings exactly once *)
let test_own_return_none_nested_cleanup () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn early_none() -> Option<i64> {
  let outer = Resource { name: "outer" };
  {
    let inner = Resource { name: "inner" };
    println("before-none");
    return None;
  }
}

fn main() {
  let r = early_none();
  println("done");
}
|}
  in
  compile_and_check
    ~expected_output:"before-none\ndrop:inner\ndrop:outer\ndone\n" src
  |> ignore

(* ======== Loop break/continue cleanup tests ======== *)

(* Loop break cleans up loop-body Drop bindings exactly once *)
let test_own_loop_break_cleanup () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn test_loop() {
  let outer = Resource { name: "outer" };
  let mut i = 0;
  while i < 3 {
    let a = Resource { name: "loop-body" };
    if i == 1 {
      break;
    }
    println("iter");
    i = i + 1;
  }
  println("after-loop");
}

fn main() {
  test_loop();
}
|}
  in
  compile_and_check
    ~expected_output:
      "iter\ndrop:loop-body\ndrop:loop-body\nafter-loop\ndrop:outer\n"
    src
  |> ignore

(* Loop continue cleans up loop-body Drop bindings exactly once *)
let test_own_loop_continue_cleanup () =
  let src =
    {|
trait Drop {
  fn drop(&mut self);
}

struct Resource {
  pub name: str,
}

impl Drop for Resource {
  fn drop(&mut self) {
    println("drop:" + self.name);
  }
}

fn test_loop() {
  let outer = Resource { name: "outer" };
  let mut i = 0;
  while i < 3 {
    let a = Resource { name: "loop-body" };
    if i == 1 {
      i = i + 1;
      continue;
    }
    println("iter");
    i = i + 1;
  }
  println("after-loop");
}

fn main() {
  test_loop();
}
|}
  in
  compile_and_check
    ~expected_output:
      "iter\n\
       drop:loop-body\n\
       drop:loop-body\n\
       iter\n\
       drop:loop-body\n\
       after-loop\n\
       drop:outer\n"
    src
  |> ignore

(* ======== Test registration ======== *)

let () =
  Alcotest.run "codegen-ownership"
    [
      ( "ownership-positive",
        [
          Alcotest.test_case "copy i64 survives assignment and call" `Quick
            test_own_copy_i64_runtime;
          Alcotest.test_case "copy bool survives call" `Quick
            test_own_copy_bool_runtime;
          Alcotest.test_case "copy struct survives call" `Quick
            test_own_copy_struct_runtime;
          Alcotest.test_case "clone escape hatch runtime" `Quick
            test_own_clone_runtime;
        ] );
      ( "ownership-negative",
        [
          Alcotest.test_case "non-Copy assign moves source" `Quick
            test_own_assign_move_negative;
          Alcotest.test_case "non-Copy call moves argument" `Quick
            test_own_call_move_negative;
          Alcotest.test_case "Drop + Copy overlap rejected" `Quick
            test_own_drop_copy_overlap;
          Alcotest.test_case "clone on non-Clone rejected" `Quick
            test_own_clone_no_impl;
          Alcotest.test_case "partial move from field via let" `Quick
            test_own_partial_move_let_negative;
          Alcotest.test_case "partial move from field via call" `Quick
            test_own_partial_move_call_negative;
        ] );
      ( "ownership-cleanup",
        [
          Alcotest.test_case "normal exit reverse-order exactly-once" `Quick
            test_own_cleanup_normal_exit;
          Alcotest.test_case "early return exactly-once" `Quick
            test_own_cleanup_early_return;
          Alcotest.test_case "nested ? exit exactly-once" `Quick
            test_own_cleanup_question_exit;
          Alcotest.test_case "overwrite cleanup exactly once" `Quick
            test_own_cleanup_overwrite;
          Alcotest.test_case "by-value return transfers ownership" `Quick
            test_own_cleanup_byval_return;
          Alcotest.test_case "callee-exit cleanup for by-value params" `Quick
            test_own_cleanup_callee_param;
          Alcotest.test_case "nested block cleanup at block exit" `Quick
            test_own_nested_block_cleanup;
          Alcotest.test_case "loop body cleanup each iteration" `Quick
            test_own_loop_cleanup;
          Alcotest.test_case "return Ok cleans nested Drop" `Quick
            test_own_return_ok_nested_cleanup;
          Alcotest.test_case "return Err cleans nested Drop" `Quick
            test_own_return_err_nested_cleanup;
          Alcotest.test_case "return Some cleans nested Drop" `Quick
            test_own_return_some_nested_cleanup;
          Alcotest.test_case "return None cleans nested Drop" `Quick
            test_own_return_none_nested_cleanup;
          Alcotest.test_case "loop break cleans loop-body Drop" `Quick
            test_own_loop_break_cleanup;
          Alcotest.test_case "loop continue cleans loop-body Drop" `Quick
            test_own_loop_continue_cleanup;
          Alcotest.test_case "let init consume suppresses guard once" `Quick
            test_own_let_init_consume_suppresses;
          Alcotest.test_case "return consume suppresses guard once" `Quick
            test_own_return_consume_suppresses;
          Alcotest.test_case "stdlib call does not suppress cleanup" `Quick
            test_own_stdlib_call_no_suppress;
        ] );
      ( "non-consuming-call-guards",
        [
          Alcotest.test_case "println does not suppress Drop cleanup" `Quick
            test_own_non_consuming_println_cleanup;
          Alcotest.test_case "println with method call keeps cleanup" `Quick
            test_own_non_consuming_println_binding;
          Alcotest.test_case "consuming call still suppresses guard" `Quick
            test_own_consuming_call_still_suppresses;
          Alcotest.test_case "mixed consuming and non-consuming" `Quick
            test_own_mixed_consuming_non_consuming;
        ] );
      ( "generic-ownership",
        [
          Alcotest.test_case "generic Drop cleanup exactly-once" `Quick
            test_own_generic_drop_cleanup;
          Alcotest.test_case "generic move transfers cleanup" `Quick
            test_own_generic_move_cleanup;
          Alcotest.test_case "generic Copy struct reusable" `Quick
            test_own_generic_copy_runtime;
          Alcotest.test_case "generic two instantiations cleanup" `Quick
            test_own_generic_two_instantiations;
          Alcotest.test_case "generic clone escape hatch" `Quick
            test_own_generic_clone_runtime;
          Alcotest.test_case "generic enum Drop cleanup once" `Quick
            test_own_generic_enum_drop;
          Alcotest.test_case "generic enum move transfers cleanup" `Quick
            test_own_generic_enum_move;
          Alcotest.test_case "generic enum Copy reusable" `Quick
            test_own_generic_enum_copy;
          Alcotest.test_case "generic enum two instantiations" `Quick
            test_own_generic_enum_clone;
          Alcotest.test_case "generic enum Clone with Self lowering" `Quick
            test_own_generic_enum_clone_runtime;
          Alcotest.test_case "generic enum payload mismatch rejected" `Quick
            test_own_generic_enum_mismatch_negative;
          Alcotest.test_case "nested generic enum payload preserves type" `Quick
            test_own_generic_enum_nested_payload;
          Alcotest.test_case "repeated tparam conflict rejected" `Quick
            test_own_generic_enum_repeated_conflict;
          Alcotest.test_case "nested generic enum move transfers cleanup" `Quick
            test_own_generic_enum_nested_move;
        ] );
      ( "example-fixtures",
        [
          Alcotest.test_case "generic_enum_clone.rg runs" `Quick
            test_generic_enum_clone_example;
          Alcotest.test_case "generic_enum_nested.rg runs" `Quick
            test_generic_enum_nested_example;
        ] );
    ]
