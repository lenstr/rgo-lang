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

(* Helper: compile rgo source to Go, check gofmt-clean, go build, go vet, go run *)
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
      (* Check go vet *)
      let code, _, stderr =
        run_cmd (Printf.sprintf "go vet %s" (Filename.quote out))
      in
      if code <> 0 then
        Alcotest.fail
          ("go vet failed:\n" ^ stderr ^ "\nGenerated Go:\n" ^ go_src);
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

(* ---------- Example fixture regression ---------- *)

let test_result_option_example () =
  let src = read_file "../examples/result_option.rg" in
  let _go = compile_and_check src in
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
        ] );
      ( "cli-pipeline",
        [ Alcotest.test_case "source to runnable Go" `Quick test_cli_pipeline ]
      );
      ( "example-fixtures",
        [
          Alcotest.test_case "result_option.rg compiles" `Quick
            test_result_option_example;
          Alcotest.test_case "impl_methods.rg runs" `Quick
            test_impl_methods_example;
          Alcotest.test_case "shapes.rg runs" `Quick test_shapes_example;
          Alcotest.test_case "loops.rg runs" `Quick test_loops_example;
        ] );
      ( "generics",
        [
          Alcotest.test_case "generics.rg builds and runs" `Quick
            test_generics_example_go_build;
          Alcotest.test_case "multi-param generic struct" `Quick
            test_generic_struct_codegen;
          Alcotest.test_case "generic free fn" `Quick test_generic_fn_codegen;
        ] );
    ]
