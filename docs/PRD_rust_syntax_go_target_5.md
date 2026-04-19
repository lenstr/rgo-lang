# PRD: Прототип языка программирования с Rust-подобным синтаксисом и таргетом Go

**Рабочее имя:** `rgo` (placeholder; расширение файлов `.rg`)
**Имплементационный язык:** OCaml 5.x
**Таргет компиляции:** Go 1.26+
**Статус:** прототип / proof-of-concept

---

## 1. Обзор

Проект — транспилятор нового языка программирования с синтаксисом, близким к Rust, но с семантикой и рантаймом Go. Пользователь пишет код на `rgo`, компилятор переводит его в идиоматичный Go-код, который затем компилируется штатным `go build`.

Основная ценность — получить Rust-подобный DX (алгебраические типы, `Option`/`Result`, `?`, pattern matching, современный тайпчекер) поверх зрелого рантайма Go (goroutines, GC, отличная стандартная библиотека, быстрая компиляция, статическая линковка).

**Не** Rust-компилятор в Go. **Не** полноценный ML-язык. Это практичный прагматичный гибрид.

---

## 2. Цели и не-цели

### Цели MVP

- Полный pipeline `.rg` → `.go` для подмножества языка.
- Синтаксис, максимально близкий к Rust для типов, функций, выражений, pattern matching.
- Поддержка `struct`, `enum` (sum types), `Option<T>`, `Result<T, E>`, оператора `?`.
- Статическая типизация с локальным выводом типов (`let x = ...`), обязательные аннотации на сигнатурах функций.
- Exhaustiveness check для `match`.
- Сгенерированный Go-код должен быть читаемым и проходить `gofmt` без изменений.
- CLI-утилита `rgoc file.rg -o out.go`.

### Не-цели (explicit out of scope)

- Собственный рантайм или GC — используем Go runtime как есть.
- Сборщик пакетов, система модулей с версионированием.
- FFI с C или другими языками.
- Борроу-чекер, lifetimes, ownership.
- Async/await (пользователь пишет goroutines напрямую через stdlib-биндинги).
- Макросы.
- Self-hosting.
- IDE-интеграция (LSP) — на будущее.

**Внутри trait-системы не-цели MVP:**
- Associated types (`type Output;` внутри trait).
- Associated constants.
- Trait objects (`dyn Trait`) — используем только generic-bounds.
- Supertraits (`trait A: B { }`).
- Trait-методы на built-in типах из prelude (`impl MyTrait for Vec<T>` — запрещено в MVP).
- Coherence / orphan rules — не актуальны при текущем однофайловом scope'е.

---

## 3. Технический стек (OCaml toolchain)

**Таргет-требование:** Go **1.26+** обязателен. Причина — self-referential type constraints (`type T[A T[A]] interface { ... }`), разрешённые только начиная с 1.26; это единственный способ корректно замаппить `Self` из trait-параметров без обходных путей. Если компилятор обнаруживает `go version` < 1.26 на системе, он предупреждает при запуске.

Использовать **только современный OCaml toolchain**:

- **OCaml 5.1+** (последний stable на момент старта).
- **opam 2.2+** — package manager; проект использует локальный switch.
- **dune 3.15+** — build system; никаких `Makefile`, `ocamlbuild`, `jbuilder`.
- **Menhir** — LR(1) парсер-генератор.
- **sedlex** — Unicode-aware лексер (предпочтительнее `ocamllex` для работы с идентификаторами и строками).
- **ppx_deriving** + **ppx_deriving_show** — автогенерация `show`/`eq` для AST-узлов (критично для отладки).
- **ppx_expect** или **alcotest** — тестирование. Рекомендуется **alcotest** для unit-тестов и **ppx_expect** для snapshot-тестов кодогенерации.
- **ocamlformat** — форматирование; `.ocamlformat` в корне проекта, profile = `default`, version зафиксирована.
- **ocaml-lsp-server** — для разработки (в dev-deps).
- **bisect_ppx** (опционально) — coverage.

**Запрещено:**
- `Core` / `Base` от Jane Street — используем stdlib OCaml. Причина: меньше зависимостей, проще читать код агенту/новому контрибьютору.
- Любые deprecated инструменты (`camlp4`, `ocamlbuild`, `jbuilder`).

### Зависимости (dune-project / *.opam)

```
(depends
  (ocaml (>= 5.1))
  (dune (>= 3.15))
  (menhir (>= 3.0))
  (sedlex (>= 3.2))
  (ppx_deriving (>= 5.2))
  (alcotest :with-test)
  (ppx_expect :with-test)
  (ocamlformat :with-dev-setup))
```

---

## 4. Спецификация языка

### 4.1. Лексика

- Идентификаторы: `[a-zA-Z_][a-zA-Z0-9_]*`, Unicode letters разрешены в идентификаторах (через sedlex).
- Ключевые слова: `fn`, `let`, `mut`, `if`, `else`, `match`, `return`, `struct`, `enum`, `true`, `false`, `for`, `while`, `loop`, `break`, `continue`, `in`, `as`, `pub`, `use`, `mod`.
- Типы-ключевые слова: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `bool`, `str`, `String`, `Option`, `Result`, `Vec`, `HashMap`.
- Комментарии: `//` однострочные, `/* */` многострочные с поддержкой вложенности.
- Литералы: целочисленные (`42`, `0xFF`, `0b101`, `1_000_000`), float (`3.14`, `1e10`), строки (`"..."` с escape-последовательностями), байтовые строки не поддерживаются в MVP.
- Операторы: `+ - * / % == != < > <= >= && || ! = += -= *= /= -> => :: . , ; ( ) { } [ ] ? & * :`.

### 4.2. Синтаксис (EBNF-like, упрощённо)

```
program      := item*
item         := fn_decl | struct_decl | enum_decl | impl_decl | impl_trait_decl
              | trait_decl | use_decl

fn_decl      := 'pub'? 'fn' IDENT generics? '(' params? ')' ('->' type)? block
fn_sig       := 'fn' IDENT generics? '(' params? ')' ('->' type)? ';'
params       := param (',' param)*
param        := 'mut'? IDENT ':' type
              | self_param                    // только первый параметр метода
self_param   := 'self' | '&' 'self' | '&' 'mut' 'self'
generics     := '<' type_param (',' type_param)* '>'
type_param   := IDENT (':' trait_bound)?
trait_bound  := IDENT ('+' IDENT)*            // T: Foo + Bar

struct_decl  := 'pub'? 'struct' IDENT generics? '{' field (',' field)* ','? '}'
field        := 'pub'? IDENT ':' type

enum_decl    := 'pub'? 'enum' IDENT generics? '{' variant (',' variant)* ','? '}'
variant      := IDENT ('(' type (',' type)* ')')?
              | IDENT '{' field (',' field)* ','? '}'

impl_decl    := 'impl' generics? type '{' fn_decl* '}'

trait_decl   := 'pub'? 'trait' IDENT generics? '{' trait_item* '}'
trait_item   := fn_sig                          // required method
              | fn_decl                         // method with default body

impl_trait_decl := 'impl' generics? IDENT 'for' type '{' fn_decl* '}'

type         := IDENT ('<' type (',' type)* '>')?
              | '&' type
              | '(' type (',' type)* ')'
              | 'Self'                         // только внутри impl/trait-блока

stmt         := 'let' 'mut'? IDENT (':' type)? '=' expr ';'
              | expr ';'
              | expr                          // final expr без ';' = value блока

expr         := literal | IDENT | binop | call | field_access
              | match_expr | if_expr | block | return_expr
              | struct_literal | enum_construct | question_expr
              | array_literal | index_expr

array_literal := '[' ']'                         // пустой, тип берётся из аннотации
              | '[' expr (',' expr)* ','? ']'   // перечисление
              | '[' expr ';' expr ']'            // повтор: значение; длина

index_expr   := expr '[' expr ']'                // v[i]

match_expr   := 'match' expr '{' arm (',' arm)* ','? '}'
arm          := pattern '=>' expr

pattern      := IDENT                         // binding
              | '_'                           // wildcard
              | literal                       // literal pattern
              | IDENT '(' pattern (',' pattern)* ')'  // tuple variant
              | IDENT '{' field_pat (',' field_pat)* '}'  // struct variant
              | IDENT '::' IDENT (...)        // qualified variant

question_expr := expr '?'
```

### 4.3. Типовая система

- Номинальная типизация для struct/enum/trait.
- Локальный вывод типов для `let`-биндингов.
- Обязательные аннотации для параметров функций и возвращаемых типов.
- Generics мономорфизация **не** делается — напрямую мапятся в Go-дженерики (Go 1.18+).
- **Trait bounds** на generic-параметрах: `<T: Foo>`, `<T: Foo + Bar>`. Проверяются тайпчекером: при вызове обобщённой функции с аргументом типа `T`, тайпчекер верифицирует, что для `T` есть все требуемые `impl Trait for T`-блоки.
- Базовые типы `i8..i64`, `u8..u64`, `f32`, `f64`, `bool`, `str` (неизменяемая строка, мапится в Go `string`).
- **Нет** lifetime'ов, **нет** борроу-чекера. `&T` просто означает "указатель/ссылку" и мапится в Go `*T` (или pass-by-reference для slices/maps автоматически).
- Exhaustiveness check для `match` — обязателен. Неполный match = compile error.
- `Self` в теле `impl`/`trait` заменяется на имя реализующего типа.

### 4.4. Примеры программ

**Hello world:**
```rust
fn main() {
    println("Hello, world!");
}
```

**Struct + метод (пока без методов в MVP, только функции):**
```rust
struct Point {
    x: f64,
    y: f64,
}

fn distance(a: Point, b: Point) -> f64 {
    let dx = a.x - b.x;
    let dy = a.y - b.y;
    sqrt(dx * dx + dy * dy)
}
```

**Enum + match:**
```rust
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
```

**Option / Result / ?:**
```rust
fn parse_int(s: str) -> Result<i64, str> {
    // биндинг к strconv.ParseInt
}

fn double(s: str) -> Result<i64, str> {
    let n = parse_int(s)?;
    Ok(n * 2)
}

fn first(v: Vec<i64>) -> Option<i64> {
    if v.len() == 0 { return None; }
    Some(v[0])
}
```

**Impl-блок с методами:**
```rust
struct Point { x: f64, y: f64 }

impl Point {
    pub fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }

    pub fn distance(&self, other: Point) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        sqrt(dx * dx + dy * dy)
    }

    pub fn translate(&mut self, dx: f64, dy: f64) {
        self.x = self.x + dx;
        self.y = self.y + dy;
    }
}

fn main() {
    let mut p = Point::new(0.0, 0.0);
    p.translate(3.0, 4.0);
    let q = Point::new(0.0, 0.0);
    println(p.distance(q));  // 5.0
}
```

**Trait и обобщённая функция:**
```rust
trait Display {
    fn display(&self) -> String;
}

trait Summary {
    fn summary(&self) -> String;
    fn short(&self) -> String {             // default-реализация
        self.summary()
    }
}

struct Article { title: String, body: String }

impl Display for Article {
    fn display(&self) -> String {
        self.title + ": " + self.body
    }
}

impl Summary for Article {
    fn summary(&self) -> String {
        self.title
    }
    // short() берётся из default
}

fn print_all<T: Display + Summary>(items: Vec<T>) {
    for item in items {
        println(item.summary());
        println(item.display());
    }
}
```

---

## 5. Маппинг в Go (критическая секция)

Это **главная** часть PRD для кодогенератора. Каждый паттерн должен быть зафиксирован и покрыт snapshot-тестами.

### 5.1. Модули и функции

- Один `.rg` файл = один Go-package. Имя package — имя файла без расширения.
- `pub fn` → `func Name(...)` (capitalized).
- `fn` (private) → `func name(...)` (lowercase).
- `fn main()` в файле с name `main` → `func main()` в package `main`.

**Пример:**
```rust
pub fn add(a: i64, b: i64) -> i64 { a + b }
```
→
```go
func Add(a int64, b int64) int64 {
    return a + b
}
```

### 5.2. Примитивные типы

| rgo      | Go          |
|----------|-------------|
| `i8..i64`  | `int8..int64` |
| `u8..u64`  | `uint8..uint64` |
| `f32`, `f64` | `float32`, `float64` |
| `bool`   | `bool`      |
| `str`, `String` | `string` |
| `Vec<T>` | `[]T`       |
| `HashMap<K,V>` | `map[K]V` |

### 5.3. Struct

```rust
pub struct User {
    pub name: String,
    age: i32,
}
```
→
```go
type User struct {
    Name string  // pub → exported
    age  int32   // private → unexported
}
```

### 5.4. Enum (sum type) — **ключевое дизайн-решение**

Используем **sealed interface pattern**. Это идиоматично для Go и хорошо работает с type switch.

```rust
pub enum Shape {
    Circle(f64),
    Rectangle { width: f64, height: f64 },
    Empty,
}
```
→
```go
type Shape interface {
    isShape()
}

type ShapeCircle struct {
    Field0 float64
}
func (ShapeCircle) isShape() {}

type ShapeRectangle struct {
    Width  float64
    Height float64
}
func (ShapeRectangle) isShape() {}

type ShapeEmpty struct{}
func (ShapeEmpty) isShape() {}
```

**Правила:**
- Имя concrete-типа: `<EnumName><VariantName>`.
- Безымянные поля варианта → `Field0`, `Field1`, ...
- Именованные поля → CamelCase идентификаторы.
- Sealed через неэкспортируемый метод `is<EnumName>()`.

### 5.5. Option<T> — гибридный маппинг по nullability

**Проблема uniform pointer-подхода.** Если `Option<T>` всегда мапится в `*T`, то nullable типы (`&U`, `Vec<U>`, `HashMap<K,V>`, enum) создают ambiguity: `&Point` и `Option<&Point>` оба дают `*Point`; `Option<Vec<T>>` становится странным `*[]T`; `Option<Option<T>>` рвётся. Поэтому используем **разную repr в зависимости от nullability `T`**, но одинаковую везде (field, return, param, element) для одного T.

**Классификация типов rgo по nullability в Go:**

| rgo type | Go type | Nullable? |
|----------|---------|-----------|
| `i8..i64`, `u8..u64`, `f32`, `f64`, `bool`, `str`/`String` | примитивы, `string` | non-nullable |
| `struct TypeName { ... }` | `TypeName` (value) | non-nullable |
| `&T` | `*T` | **nullable** |
| `Vec<T>` | `[]T` | **nullable** |
| `HashMap<K, V>` | `map[K]V` | **nullable** |
| `enum TypeName { ... }` | `TypeName` (sealed interface) | **nullable** |

**Правила маппинга `Option<T>`:**

**Случай A — `T` non-nullable (primitive, struct, string):**

`Option<T>` → `*T`

- `None` → `nil`.
- `Some(expr)` → `new(expr)` — используем Go 1.26+ built-in `new(expr)`, который принимает выражение и возвращает `*T`. Никаких IIFE-обёрток не нужно.
- Проверка: `opt == nil` (None) / `opt != nil` (Some).
- Извлечение значения: `*opt`.

```rust
fn find(xs: Vec<i64>, target: i64) -> Option<i64> { ... }
let x = find(v, 42);
```
→
```go
func find(xs []int64, target int64) *int64 { ... }
x := find(v, 42)
```

И внутри `find`:
```rust
if xs[i] == target { return Some(i); }
return None;
```
→
```go
if xs[i] == target { return new(i) }
return nil
```

**Случай B — `T` nullable (`&U`, `Vec`, `HashMap`, enum):**

`Option<T>` → `Option[T]` — generic struct, генерируемый один раз в prelude выходного файла:

```go
type Option[T any] struct {
    some  bool
    value T
}

func rgo_some[T any](v T) Option[T] { return Option[T]{some: true, value: v} }
func rgo_none[T any]() Option[T]    { return Option[T]{} }
```

- `None` → `rgo_none[T]()`.
- `Some(expr)` → `rgo_some[T](expr)` (тип-аргумент обычно выводится).
- Проверка: `opt.some`.
- Извлечение значения: `opt.value`.

```rust
fn best_cache() -> Option<HashMap<String, i64>> { ... }
```
→
```go
func bestCache() Option[map[string]int64] { ... }
```

**Преимущества гибрида:**

- `&Point` (это `*Point`) и `Option<&Point>` (это `Option[*Point]`) — разные Go-типы, не смешиваются.
- `Option<Vec<T>>` → `Option[[]T]` — отличается от просто `[]T`.
- `Option<Option<T>>` → работает: внешний выбор зависит от внутреннего представления.
- Одинаковая repr внутри functions, fields, params, Vec elements.

**Маппинг методов stdlib, возвращающих Option:**

Для идиоматической Go-интероперабельности внутри compiler'а преобразуем `v, ok := m[k]` в `Option[V]`:

| rgo | Go codegen |
|-----|-----------|
| `m.get(k)` где `V` non-nullable | `func() *V { v, ok := m[k]; if !ok { return nil }; return &v }()` |
| `m.get(k)` где `V` nullable | `func() Option[V] { v, ok := m[k]; if !ok { return rgo_none[V]() }; return rgo_some(v) }()` |

Эти IIFE обычно inline'ятся Go-компилятором. Читаемость выходного кода — приемлемая.

### 5.6. Result<T, E> → `(T, error)`

```rust
fn divide(a: i64, b: i64) -> Result<i64, str> {
    if b == 0 { return Err("division by zero"); }
    Ok(a / b)
}
```
→
```go
func divide(a int64, b int64) (int64, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}
```

**Правила:**
- Тип `E` **должен** быть маппим в Go `error`. Для MVP: разрешить только `E = str`, которое маппится в `error` через `errors.New`. Позже — разрешить пользовательские типы, реализующие `error` интерфейс.
- `Ok(v)` → `return v, nil` (в контексте return) или `v, error(nil)` (в выражении — редко нужно).
- `Err(e)` → `return zero_value_of_T, errors.New(e)` или `return zero, e` если `e` уже `error`.
- Zero value для `T` генерируется по таблице (0 для чисел, `""` для string, `nil` для указателей/интерфейсов, `<TypeName>{}` для struct).

### 5.7. Оператор `?` (error propagation)

Поведение зависит от типа enclosing функции и типа выражения.

**Случай 1: `?` на `Result<T, E>` в функции, возвращающей `Result<U, E>`:**
```rust
fn foo() -> Result<i64, str> {
    let n = bar()?;
    Ok(n * 2)
}
```
→
```go
func foo() (int64, error) {
    n, err := bar()
    if err != nil {
        return 0, err
    }
    return n * 2, nil
}
```

**Случай 2: `?` на `Option<T>` в функции, возвращающей `Option<U>` — зависит от nullability T:**

**Подслучай 2a — T non-nullable (`Option<T>` это `*T`):**
```rust
fn foo() -> Option<i64> {
    let n = bar()?;
    Some(n * 2)
}
```
→
```go
func foo() *int64 {
    n := bar()
    if n == nil {
        return nil
    }
    return new((*n) * 2)
}
```

**Подслучай 2b — T nullable (`Option<T>` это `Option[T]`):**
```rust
fn foo() -> Option<Vec<i64>> {
    let v = bar()?;   // bar returns Option<Vec<i64>>
    Some(v)
}
```
→
```go
func foo() Option[[]int64] {
    __tmp := bar()
    if !__tmp.some {
        return rgo_none[[]int64]()
    }
    v := __tmp.value
    return rgo_some(v)
}
```

**Случай 3: `?` в несовместимом контексте** → compile error в нашем тайпчекере.

**Генерация имён временных переменных:** использовать монотонный счётчик `__rgo_tmp_N`, либо префикс по типу: `__rgo_err_N`, `__rgo_opt_N`. Важно, чтобы сгенерированные имена не конфликтовали с пользовательскими.

### 5.8. Pattern matching → Go type switch

```rust
match s {
    Shape::Circle(r) => r * r * 3.14,
    Shape::Rectangle { width, height } => width * height,
    Shape::Empty => 0.0,
}
```
→
```go
func() float64 {
    switch __v := s.(type) {
    case ShapeCircle:
        r := __v.Field0
        return r * r * 3.14
    case ShapeRectangle:
        width := __v.Width
        height := __v.Height
        return width * height
    case ShapeEmpty:
        _ = __v
        return 0.0
    default:
        panic("unreachable: non-exhaustive match")
    }
}()
```

**Обёртка в IIFE** (immediately-invoked function expression) нужна, потому что `match` в rgo — это выражение, а Go `switch` — statement. Для `match` в statement-позиции IIFE не нужен.

Exhaustiveness проверяется до кодогенерации. `default: panic` добавляется как defensive measure (должен быть unreachable).

### 5.9. Generics

```rust
fn first<T>(v: Vec<T>) -> Option<T> { ... }
```
→
```go
func first[T any](v []T) *T { ... }
```

Прямой маппинг. Для MVP поддерживаем только `any`-bound (без trait bounds).

### 5.10. Контрольный поток

- `if/else` → `if/else` (выражение → IIFE если нужно получить значение).
- `loop { }` → `for { }`.
- `while cond { }` → `for cond { }`.
- `for x in iter { }` → `for _, x := range iter { }` (для slices); для остального — out of scope MVP.
- `break`, `continue`, `return` — прямой маппинг.

### 5.11. Стандартная библиотека (минимальная)

Для MVP требуется минимальный "prelude". Все методы и функции ниже специально обрабатываются кодогенератором — это не обычные пользовательские impl-блоки.

**I/O и math:**
| rgo | Go |
|-----|-----|
| `println(s)` | `fmt.Println(s)` |
| `print(s)` | `fmt.Print(s)` |
| `sqrt(x: f64) -> f64` | `math.Sqrt(x)` |
| `abs(x: f64) -> f64` | `math.Abs(x)` |

**`Vec<T>`** — маппится в `[]T`:

| rgo | Go |
|-----|-----|
| `Vec::new() -> Vec<T>` | `[]T{}` (T выводится из контекста) |
| `Vec::with_capacity(n: i64) -> Vec<T>` | `make([]T, 0, n)` |
| `v.len() -> i64` | `int64(len(v))` |
| `v[i]` (чтение) | `v[i]` (Go паникует при OOB, как и Rust) |
| `v[i] = x` (запись) | `v[i] = x` |
| `v.push(x: T)` (на `&mut self`) | `v = append(v, x)` — требует pointer receiver |
| `v.pop() -> Option<T>` | специальный inline-код с проверкой на пустоту |
| `for x in v { }` | `for _, x := range v { }` |

Важно: `push` мутирует, поэтому переменная должна быть объявлена `let mut v = ...`. Тайпчекер проверяет мутабельность.

**Array/Vec литералы (обязательная часть MVP):**

Rust-подобный синтаксис литералов массивов в rgo создаёт `Vec<T>` (мы не разделяем fixed-size arrays и Vec — это намеренно):

| rgo | Go | Примечание |
|-----|-----|-----------|
| `[1, 2, 3]` | `[]int64{1, 2, 3}` | перечисление; T выводится из элементов |
| `[]` (с аннотацией `: Vec<T>`) | `[]T{}` | пустой литерал |
| `[x; n]` (n — const integer) | `[]T{x, x, ..., x}` | раскрытие в compile-time, только для малых const n (≤ 16) |
| `[x; n]` (n — expr, или большой n) | вызов helper'а `rgo_repeat[T](x, n)` | runtime-раскрытие |

Helper `rgo_repeat` генерируется один раз в prelude выходного файла:

```go
func rgo_repeat[T any](x T, n int64) []T {
    result := make([]T, n)
    for i := range result {
        result[i] = x
    }
    return result
}
```

Тайпчекер:
- `[]` без аннотации типа → ошибка "cannot infer element type for empty array literal".
- Элементы в перечислении должны иметь один и тот же тип (по унификации).
- В `[x; n]` — `n` обязан быть `i64` (или `i32`, с авто-конверсией в `int64`).

**Примеры:**

```rust
let xs: Vec<i64> = [1, 2, 3];           // → []int64{1, 2, 3}
let ys = [1, 2, 3];                      // тип выводится: Vec<i64>
let zeros: Vec<i64> = [0; 100];          // → rgo_repeat[int64](0, 100)
let empty: Vec<String> = [];             // → []string{}

for v in [3, 7, 8] {                     // итерация по литералу массива
    println(v);
}

let nested: Vec<Vec<i64>> = [[1, 2], [3, 4]];  // литералы вложенные
```

**`HashMap<K, V>`** — маппится в `map[K]V`:

| rgo | Go |
|-----|-----|
| `HashMap::new() -> HashMap<K,V>` | `map[K]V{}` |
| `m.len() -> i64` | `int64(len(m))` |
| `m.insert(k: K, v: V)` (на `&mut self`) | `m[k] = v` |
| `m.get(k: K) -> Option<V>` | `func() *V { v, ok := m[k]; if !ok { return nil }; return &v }()` |
| `m.contains_key(k: K) -> bool` | `_, ok := m[k]; ok` (как выражение через IIFE) |
| `m.remove(k: K)` (на `&mut self`) | `delete(m, k)` |
| `for (k, v) in m { }` | `for k, v := range m { }` |

Ключевой тип `K` должен быть Go-comparable (int, string, bool, pointer, struct из comparable полей). Для MVP — проверка доверяется Go-компилятору (если некорректно, ошибка будет при `go build`). Пользовательская проверка на стороне rgo — следующая фаза.

**`String` / `str`** — оба маппятся в Go `string`:

| rgo | Go |
|-----|-----|
| `s.len() -> i64` | `int64(len(s))` |
| `s1 + s2` | `s1 + s2` |
| `String::from(x)` | зависит от `x`; для `str` — identity, для чисел — `strconv.Itoa` и т.п. |

### 5.12. `impl`-блоки и методы

Блок `impl TypeName { ... }` содержит методы и ассоциированные функции типа. Поддерживается impl как на struct, так и на enum. Несколько impl-блоков для одного типа разрешены — объединяются при кодогенерации.

**Самоидентификаторы (`self`-параметры):**

| rgo | Go-семантика |
|-----|--------------|
| `fn foo(self, ...)` | value receiver: `func (self T) Foo(...)` |
| `fn foo(&self, ...)` | pointer receiver: `func (self *T) Foo(...)` |
| `fn foo(&mut self, ...)` | pointer receiver: `func (self *T) Foo(...)` |
| `fn foo(...)` (без self) | ассоциированная функция, см. ниже |

В MVP `&self` и `&mut self` мапятся одинаково (pointer receiver). Тайпчекер **проверяет** мутабельность: вызов метода `&mut self` требует `let mut` и/или `&mut` биндинга receiver'а. Это сохраняет rgo-level дисциплину, хотя Go её не enforce'ит.

**Ассоциированные функции (без self):**

`TypeName::method_name` → top-level Go-функция с именем `<TypeName><MethodName>`:
- `Point::new` (pub) → `PointNew`
- `Point::private_helper` (private) → `pointPrivateHelper`

**Ключевое слово `Self`:**

`Self` в теле impl заменяется на имя типа. Разрешено везде: в типах полей параметров, в возвращаемом типе, в теле метода.

**Импл на struct:**

```rust
struct Counter { value: i64 }

impl Counter {
    pub fn new() -> Self { Counter { value: 0 } }
    pub fn get(&self) -> i64 { self.value }
    pub fn inc(&mut self) { self.value = self.value + 1 }
}
```
→
```go
type Counter struct {
    Value int64
}

func CounterNew() Counter {
    return Counter{Value: 0}
}

func (self *Counter) Get() int64 {
    return self.Value
}

func (self *Counter) Inc() {
    self.Value = self.Value + 1
}
```

**Импл на enum:**

Go не позволяет добавлять методы к уже объявленному interface-типу извне, поэтому методы enum'а кодогенерируются следующим образом:

1. Сигнатуры методов добавляются в sealed interface.
2. Генерируется shared helper-функция с телом метода, принимающая enum по значению.
3. Каждый вариант получает метод-делегатор, вызывающий helper.

```rust
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
```
→
```go
type Shape interface {
    isShape()
    Area() float64
}

type ShapeCircle struct{ Field0 float64 }
func (ShapeCircle) isShape() {}

type ShapeSquare struct{ Field0 float64 }
func (ShapeSquare) isShape() {}

func shapeAreaImpl(self Shape) float64 {
    switch __v := self.(type) {
    case ShapeCircle:
        r := __v.Field0
        return 3.14 * r * r
    case ShapeSquare:
        s := __v.Field0
        return s * s
    default:
        panic("unreachable")
    }
}

func (self ShapeCircle) Area() float64 { return shapeAreaImpl(self) }
func (self ShapeSquare) Area() float64 { return shapeAreaImpl(self) }
```

Неприятная verbose'ность генерированного кода — осознанный компромисс ради сохранения call-syntax `s.area()` в rgo и идиоматичности вызова `s.Area()` в Go. Альтернатива (transform method call → function call на стороне codegen) тоже допустима; реализатор волен выбрать, но итоговая семантика должна быть идентична.

**Generic impl:**

```rust
struct Box<T> { value: T }

impl<T> Box<T> {
    pub fn new(v: T) -> Self { Box { value: v } }
    pub fn get(&self) -> T { self.value }
}
```
→
```go
type Box[T any] struct {
    Value T
}

func BoxNew[T any](v T) Box[T] {
    return Box[T]{Value: v}
}

func (self *Box[T]) Get() T {
    return self.Value
}
```

**Call-site кодогенерация:**

- `p.distance(q)` → `p.Distance(q)` если метод pub; `p.distance(q)` если private.
- `Point::new(1.0, 2.0)` → `PointNew(1.0, 2.0)`.
- Для pointer-receiver методов на не-указателях Go делает автоматический address-of — обычно это ок, но если receiver'ом выступает непересылаемое выражение, кодогенератор обязан завести временную переменную.

**Ограничения MVP:**

- `impl` только на типах, объявленных в том же файле. Orphan-rules не актуальны (традиций нет).
- Нет наследования, нет default-реализаций (для inherent impl; default'ы есть в trait'ах, см. §5.13).
- Метод не может иметь имя, конфликтующее с sealed-методом `is<EnumName>`.

### 5.13. Traits

Trait — именованный набор сигнатур методов (опционально с default-реализациями). Типы реализуют trait через `impl Trait for Type { ... }`. Мапится в Go напрямую через interface (структурная типизация Go автоматически обеспечивает satisfaction — никаких явных declarations implements не нужно).

**Декларация trait:**

```rust
trait Display {
    fn display(&self) -> String;
}

trait Summary {
    fn summary(&self) -> String;
    fn short(&self) -> String {
        self.summary()
    }
}
```
→
```go
type Display interface {
    Display() string
}

type Summary interface {
    Summary() string
    Short() string
}
```

**Важно:** default-методы **не** попадают в Go interface специальным образом — они сохраняются как сигнатуры наравне с required-методами. Генерация default-тел происходит на стороне реализующего типа (см. ниже).

**`impl Trait for Struct`:**

```rust
impl Display for Article {
    fn display(&self) -> String {
        self.title + ": " + self.body
    }
}

impl Summary for Article {
    fn summary(&self) -> String {
        self.title
    }
    // short() использует default
}
```
→
```go
func (self *Article) Display() string {
    return self.Title + ": " + self.Body
}

func (self *Article) Summary() string {
    return self.Title
}

// Default-реализация short() специализирована для Article:
func (self *Article) Short() string {
    return self.Summary()
}
```

Go-компилятор автоматически видит, что `*Article` удовлетворяет `Display` и `Summary` — никаких явных аннотаций не требуется. Тайпчекер rgo, однако, проверяет наличие всех required-методов в impl-блоке до кодогенерации.

**`impl Trait for Enum`:**

Для enum'а методы trait'а добавляются в sealed interface, и каждый вариант получает метод-делегатор к shared helper'у (аналогично inherent impl из §5.12):

```rust
trait Area {
    fn area(&self) -> f64;
}

enum Shape {
    Circle(f64),
    Square(f64),
}

impl Area for Shape {
    fn area(&self) -> f64 {
        match self {
            Shape::Circle(r) => 3.14 * r * r,
            Shape::Square(s) => s * s,
        }
    }
}
```
→
```go
type Area interface {
    Area() float64
}

type Shape interface {
    isShape()
    Area() float64          // добавлено из impl Area for Shape
}

type ShapeCircle struct{ Field0 float64 }
func (ShapeCircle) isShape() {}

type ShapeSquare struct{ Field0 float64 }
func (ShapeSquare) isShape() {}

func shapeAreaImpl(self Shape) float64 {
    switch __v := self.(type) {
    case ShapeCircle:
        r := __v.Field0
        return 3.14 * r * r
    case ShapeSquare:
        s := __v.Field0
        return s * s
    default:
        panic("unreachable")
    }
}

func (self ShapeCircle) Area() float64 { return shapeAreaImpl(self) }
func (self ShapeSquare) Area() float64 { return shapeAreaImpl(self) }
```

**Generic bounds:**

```rust
fn print_all<T: Display>(items: Vec<T>) {
    for item in items {
        println(item.display());
    }
}
```
→
```go
func PrintAll[T Display](items []T) {
    for _, item := range items {
        fmt.Println(item.Display())
    }
}
```

Множественные bounds через `+` → Go-анонимный interface-constraint:

```rust
fn foo<T: Display + Summary>(x: T) { ... }
```
→
```go
func Foo[T interface { Display; Summary }](x T) { ... }
```

Альтернативно — агент может генерировать именованный composite-interface перед функцией для читаемости, это не regression.

**`Self` в trait'ах (Go 1.26+ self-referential constraints):**

Go 1.26 снял ограничение, при котором generic-тип не мог ссылаться на себя в собственном списке type-параметров. Это позволяет маппить `Self` из Rust-трейтов напрямую, без ограничений на позицию (параметр/возврат).

Правило маппинга:
- Если trait использует `Self` **в любом положении** (параметр, возврат, вложенный тип) хотя бы в одном методе → Go-interface становится **self-referential generic**: `type TraitName[Self TraitName[Self]] interface { ... }`, все `Self` в сигнатурах заменяются на type-параметр `Self`.
- Если `Self` не используется → обычный non-generic interface.

```rust
trait Eq {
    fn eq(&self, other: &Self) -> bool;
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

trait Clone {
    fn clone(&self) -> Self;
}
```
→
```go
type Eq[Self Eq[Self]] interface {
    Eq(other *Self) bool
    Ne(other *Self) bool
}

type Clone[Self Clone[Self]] interface {
    Clone() Self
}
```

Impl-блок подставляет конкретный тип:

```rust
impl Eq for Point {
    fn eq(&self, other: &Point) -> bool {
        self.x == other.x && self.y == other.y
    }
    // ne() берётся из default
}
```
→
```go
func (self *Point) Eq(other *Point) bool {
    return self.X == other.X && self.Y == other.Y
}

func (self *Point) Ne(other *Point) bool {
    return !self.Eq(other)
}
```

`*Point` автоматически удовлетворяет `Eq[Point]` по структурной типизации Go.

**Generic bounds (с учётом self-referential traits):**

Правило подстановки при использовании trait'а как bound'а:
- Trait без `Self`: `<T: Display>` → `[T Display]`
- Trait с `Self`: `<T: Eq>` → `[T Eq[T]]` (подставляем concrete `T` в Self-позицию)
- Смешанные bounds: `<T: Display + Eq>` → `[T interface { Display; Eq[T] }]`

```rust
fn all_equal<T: Eq>(items: Vec<T>) -> bool {
    let mut i: i64 = 1;
    while i < items.len() {
        if !items[0].eq(&items[i]) { return false; }
        i = i + 1;
    }
    true
}
```
→
```go
func AllEqual[T Eq[T]](items []T) bool {
    var i int64 = 1
    for i < int64(len(items)) {
        if !items[0].Eq(&items[i]) {
            return false
        }
        i = i + 1
    }
    return true
}
```

**Правила имён в сгенерированном Go:**

- Trait `Display` (pub) → interface `Display`.
- Trait `internal_helper` (private) → interface `internalHelper`.
- Методы trait — как обычно: pub → CamelCase, private → camelCase.

**Call-site:**

- `item.display()` → `item.Display()` (если pub), `item.display()` (если private).
- Метод default'а вызывается так же, как обычный метод — Go не различает, поскольку к этому моменту он уже сгенерирован на типе.

**Что тайпчекер обязан проверять:**

1. В `impl Trait for Type` присутствуют все required-методы trait'а.
2. Сигнатуры методов в impl точно совпадают с сигнатурами в trait (с учётом подстановки `Self` → `Type`).
3. Для каждого использования `T: Trait` существует `impl Trait for T` (или bound-проверка при substitution).
4. Попытка `impl Trait for Type`, где Trait не объявлен, → ошибка.
5. Дубликаты `impl Trait for Type` → ошибка.
6. `Self` используется только внутри тел trait-declaration'а и impl-блока (вне них — ошибка).

---

## 6. Архитектура компилятора

```
source.rg
    │
    ▼
┌───────────┐
│  Lexer    │  (sedlex)
│  → tokens │
└───────────┘
    │
    ▼
┌───────────┐
│  Parser   │  (Menhir)
│  → AST    │
└───────────┘
    │
    ▼
┌──────────────┐
│ Name resolve │  (ручной проход)
│ → resolved   │
│   AST        │
└──────────────┘
    │
    ▼
┌──────────────┐
│ Type check   │  (bidirectional + local inference)
│ + exhaust.   │
│ → typed AST  │
└──────────────┘
    │
    ▼
┌──────────────┐
│  Codegen     │  (string/Buffer-based)
│  → .go file  │
└──────────────┘
    │
    ▼
out.go  →  gofmt  →  go build
```

### Ключевые модули OCaml

| Модуль | Ответственность |
|--------|-----------------|
| `Lexer` (sedlex) | Токенизация |
| `Parser` (Menhir) | Построение AST |
| `Ast` | Типы AST-узлов, `[@@deriving show]` |
| `Resolver` | Resolve имён, построение symbol table |
| `Types` | Представление типов, unification |
| `Typecheck` | Проверка и вывод типов |
| `Exhaust` | Exhaustiveness check для match |
| `Codegen` | Генерация Go |
| `Driver` | Склейка фаз, CLI |

Каждый модуль имеет свой `.ml` и `.mli` файл (интерфейс **обязателен** — дисциплинирует).

---

## 7. Структура проекта

```
rgo/
├── dune-project
├── rgo.opam
├── .ocamlformat
├── README.md
├── bin/
│   ├── dune
│   └── main.ml           # CLI entry point
├── lib/
│   ├── dune
│   ├── ast.ml            # + ast.mli
│   ├── lexer.ml          # sedlex-based
│   ├── parser.mly        # Menhir
│   ├── resolver.ml
│   ├── types.ml
│   ├── typecheck.ml
│   ├── exhaust.ml
│   ├── codegen.ml
│   └── driver.ml
├── test/
│   ├── dune
│   ├── test_lexer.ml
│   ├── test_parser.ml
│   ├── test_typecheck.ml
│   ├── test_codegen.ml
│   └── e2e/              # end-to-end tests
│       ├── hello/
│       │   ├── input.rg
│       │   └── expected.go
│       ├── enum_match/
│       └── result_propagation/
└── examples/             # sample .rg programs
    ├── hello.rg
    ├── shapes.rg
    └── parse_int.rg
```

---

## 8. Фазы разработки

Каждая фаза имеет чёткие acceptance criteria. Агент должен завершать фазу **полностью**, включая тесты, прежде чем переходить к следующей.

### Phase 0: Project skeleton

**Задачи:**
- `opam switch create . 5.1.0 --deps-only`
- `dune-project`, `rgo.opam`, `dune` файлы во всех директориях.
- `.ocamlformat` с зафиксированной версией.
- `bin/main.ml` печатает "rgoc v0.0.1".
- CI-friendly `make test` (или `dune runtest`) — даже если тестов пока нет.

**Acceptance:**
- `dune build` успешно.
- `dune exec rgoc -- --version` печатает версию.
- `dune runtest` проходит (пусто ок).

### Phase 1: Lexer

**Задачи:**
- Реализовать `Lexer` на sedlex со всеми токенами из §4.1.
- Токен включает position (line, column, offset).
- Ошибки лексера — через exception с позицией.

**Acceptance:**
- Unit-тесты на каждый тип токена (ключевые слова, идентификаторы, числа, строки, операторы, комментарии).
- Вложенные комментарии работают.
- Юникод в идентификаторах (базовый case).

### Phase 2: Parser + AST

**Задачи:**
- Описать AST в `ast.ml` с `[@@deriving show]`.
- Написать `parser.mly` на Menhir, покрывающий грамматику из §4.2.
- Pretty-printer AST для отладки (можно через show).

**Acceptance:**
- Парсинг всех примеров из `examples/`.
- Snapshot-тесты AST для каждой значимой конструкции.
- Внятные синтаксические ошибки (с номером строки).

### Phase 3: Hello world e2e (без типов)

**Задачи:**
- Временный naive codegen для функций без типов — только для `println` и арифметики.
- CLI `rgoc input.rg -o output.go`.
- Прогон `gofmt` на выход.

**Acceptance:**
- `examples/hello.rg` → `output.go` → `go run output.go` печатает "Hello, world!".

### Phase 4: Name resolution

**Задачи:**
- Symbol table: глобальные функции, типы, локальные биндинги.
- Ошибки: undefined identifier, duplicate definition.
- Резолвится всё: типы в сигнатурах, варианты enum'ов, имена полей.

**Acceptance:**
- Тесты на положительные и отрицательные кейсы.
- После фазы — каждый `Ident` в AST имеет ссылку на определение.

### Phase 5: Type checking + inference

**Задачи:**
- Представление типов: `TInt of int_size | TFloat | TBool | TString | TStruct of id | TEnum of id | TOption of ty | TResult of ty*ty | TVec of ty | TFn of ty list * ty | TVar of var_id`.
- Bidirectional type checking: `check : ctx -> expr -> ty -> unit`, `infer : ctx -> expr -> ty`.
- Unification для `let`-биндингов (простой Union-Find).
- Проверка сигнатур: параметры, возвращаемый тип, вызовы.
- Специальная логика для `?`:
  - На `Result<T, E>` в функции, возвращающей `Result<U, E>` (одинаковый `E`) → тип выражения `T`.
  - На `Option<T>` в функции, возвращающей `Option<U>` → тип выражения `T`.
  - Иначе → ошибка с внятным сообщением.

**Acceptance:**
- Все примеры из §4.4 проходят typecheck.
- Негативные тесты: несовместимые типы, `?` в неверном контексте, неверные аргументы.

### Phase 6: Exhaustiveness check

**Задачи:**
- Алгоритм Maranget (simplified) для проверки покрытия паттернов в `match`.
- Сообщения об ошибках с примером непокрытого паттерна.

**Acceptance:**
- Неполный match на enum — compile error с указанием missing variants.
- Wildcard `_` покрывает всё — ок.

### Phase 7: Full codegen (inherent impl, без trait'ов)

**Задачи:**
- Переписать `codegen.ml` полностью на основе typed AST.
- Реализовать все правила из §5.1–§5.12, включая:
  - Struct и enum (sealed interface).
  - `Option<T>` с гибридным маппингом по nullability `T`: `*T` для non-nullable, `Option[T]` struct для nullable. Generic `Option[T]` struct и хелперы `rgo_some`/`rgo_none` генерируются в prelude один раз.
  - `Some(x)` использует Go 1.26 `new(expr)` (для non-nullable T) или struct literal (для nullable).
  - `Result<T, E>`, оператор `?` — с разветвлением по случаям Option vs Result и по nullability.
  - Pattern matching через type switch.
  - **`impl`-блоки:** методы на struct (Go-методы с receiver'ом), методы на enum (sealed interface + helper + per-variant delegators), ассоциированные функции (top-level `<Type><Method>`).
  - Generics (включая generic impl), **без trait bounds** на этой фазе.
  - `Vec`/`HashMap` и их методы через специальную обработку; `HashMap::get` завёртывается в подходящий Option-вариант.
  - Array литералы (`[...]` и `[x; n]`), индексация.
- Сгенерированный код **должен** проходить `gofmt` (кодогенератор прогоняет gofmt на выходе через subprocess).
- Детерминированность: одинаковый вход → побайтово одинаковый выход.

**Acceptance:**
- Snapshot-тесты на каждый паттерн маппинга (struct, enum, Option-non-nullable (`*T`), Option-nullable (`Option[T]` struct), Option вложенные типа `Option<Option<T>>` и `Option<Vec<T>>`, Result, ?, match, generics, циклы, impl на struct, impl на enum, static methods, array литералы `[...]` и `[x; n]`, индексация `v[i]`, `Vec`/`HashMap` операции).
- E2E: `dune runtest` запускает компиляцию каждого примера, сравнивает с `expected.go`, затем `go build` и `go run` на результате.

### Phase 8: Traits

**Задачи:**
- Парсинг `trait_decl` и `impl_trait_decl`.
- Name resolution для trait'ов: trait — это namespace, методы резолвятся через trait-имя при dotted-вызове (`x.method()` → найти, какой trait обеспечивает `method` для типа `x`).
- Тайпчекер:
  - Проверка полноты impl-блока (все required-методы присутствуют).
  - Проверка совпадения сигнатур (с подстановкой `Self` → concrete type).
  - Trait bounds на generics: при вызове `f<T: Trait>(x)` с конкретным `T`, убедиться, что `impl Trait for T` существует.
  - Ошибка на дубли `impl Trait for Type`.
- Codegen по §5.13:
  - `trait X { ... }` без `Self` → обычный Go interface.
  - `trait X { ... }` с `Self` в любом методе → self-referential generic interface: `type X[Self X[Self]] interface { ... }` (Go 1.26+).
  - `impl Trait for Struct` → методы на struct (с подстановкой `Self` → `Struct`).
  - `impl Trait for Enum` → расширение sealed interface + helper + per-variant delegators.
  - Default-методы: если impl не предоставляет метод, генерируется специализированная реализация с подставленным `Self`/типом.
  - Generic bounds: `<T: A + B>` → `[T interface { A; B }]`; для self-referential trait — `[T A[T]]`.

**Acceptance:**
- Snapshot-тесты: trait declaration (с `Self` и без), impl for struct, impl for enum, default methods, generic bounds (single/multi, с self-reference).
- E2E: программа с trait'ами, default-методами и `<T: Trait>` функцией компилируется и работает.
- Работает trait `Eq` с `fn eq(&self, other: &Self) -> bool` — и generic функция `all_equal<T: Eq>`.
- Негативные тесты:
  - impl без required-метода → ошибка.
  - Несовпадение сигнатуры → ошибка.
  - Использование bounded-generic с типом без impl → ошибка.

### Phase 9: Полировка + документация

**Задачи:**
- README с quickstart.
- Документация синтаксиса (можно авто-генерируемая из Menhir-грамматики).
- CLI: `--emit-ast`, `--emit-typed`, `--no-gofmt` флаги для отладки.
- Обработка множественных ошибок (не падать на первой).

**Acceptance:**
- Свежий человек может склонировать репо, собрать, прогнать examples.

---

## 9. Стратегия тестирования

### Unit-тесты
- **alcotest** для лексера, парсера, резолвера, тайпчекера.
- Каждый модуль — свой test-suite.

### Snapshot-тесты
- **ppx_expect** для вывода AST и кодогенерации.
- Обновляются через `dune runtest --auto-promote`.

### End-to-end тесты
- Структура `test/e2e/<name>/{input.rg, expected.go, expected.stdout}`.
- Тест-ранер:
  1. Компилирует `input.rg` → `out.go`.
  2. Сравнивает с `expected.go` (после нормализации через gofmt).
  3. `go run out.go` → сравнивает stdout с `expected.stdout`.

### Negative-тесты
- `test/negative/<name>/{input.rg, expected_error.txt}`.
- Проверка, что компилятор выдаёт **именно** ожидаемую ошибку.

---

## 10. Критерии приёмки MVP

Проект считается MVP-готовым, когда:

1. Все фазы 0–8 завершены, все тесты зелёные.
2. Следующая программа компилируется и работает корректно:
   ```rust
   trait Display {
       fn display(&self) -> String;
   }

   enum Tree<T> {
       Leaf,
       Node { value: T, left: Tree<T>, right: Tree<T> },
   }

   impl<T> Tree<T> {
       pub fn empty() -> Self { Tree::Leaf }
   }

   fn insert(t: Tree<i64>, v: i64) -> Tree<i64> {
       match t {
           Tree::Leaf => Tree::Node { value: v, left: Tree::Leaf, right: Tree::Leaf },
           Tree::Node { value, left, right } => {
               if v < value {
                   Tree::Node { value, left: insert(left, v), right }
               } else {
                   Tree::Node { value, left, right: insert(right, v) }
               }
           }
       }
   }

   fn find(t: Tree<i64>, v: i64) -> Option<i64> {
       match t {
           Tree::Leaf => None,
           Tree::Node { value, left, right } => {
               if v == value { Some(value) }
               else if v < value { find(left, v) }
               else { find(right, v) }
           }
       }
   }

   struct Counter { hits: i64, misses: i64 }

   impl Counter {
       pub fn new() -> Self { Counter { hits: 0, misses: 0 } }

       pub fn record(&mut self, found: bool) {
           if found { self.hits = self.hits + 1; }
           else { self.misses = self.misses + 1; }
       }
   }

   impl Display for Counter {
       fn display(&self) -> String {
           String::from("Counter(hits=") + String::from(self.hits)
               + String::from(", misses=") + String::from(self.misses) + String::from(")")
       }
   }

   fn print_line<T: Display>(x: T) {
       println(x.display());
   }

   fn main() {
       let mut t = Tree::empty();
       let mut cache: HashMap<i64, i64> = HashMap::new();
       let mut stats = Counter::new();

       let values: Vec<i64> = [5, 3, 8, 1, 9];
       for v in values {
           t = insert(t, v);
           cache.insert(v, v * 2);
       }

       for q in [3, 7, 8] {
           match find(t, q) {
               Some(_) => stats.record(true),
               None => stats.record(false),
           }
       }

       print_line(stats);
   }
   ```
3. Сгенерированный Go-код проходит `gofmt -d` без изменений.
4. `go vet` на сгенерированном коде не выдаёт warnings.
5. Тесты не флейкают (детерминированная кодогенерация).

---

## 11. Конкретные гайдлайны для агента-исполнителя

- **Коммиты:** атомарные, один commit = одна логическая единица. Сообщения в conventional commits (`feat:`, `fix:`, `test:`, `refactor:`).
- **Ветки:** одна ветка на фазу: `phase-0-skeleton`, `phase-1-lexer` и т.д.
- **Не добавлять зависимости** без явного обоснования в commit message.
- **`.mli` файлы обязательны** для всех модулей `lib/`.
- **Никакого `open` на top-level** за исключением stdlib — используйте `let module M = ... in` или полные пути.
- **Warnings as errors:** `(flags (:standard -w +a-4-40-41-42-44-45-48-58-59-60-67-68-69-70))` в dune.
- **Форматирование:** `dune build @fmt` обязан проходить перед каждым коммитом.
- **Документировать неочевидные решения** в `docs/decisions/NNNN-decision.md` (ADR).

## 12. Риски и открытые вопросы

- **`?` на generic error типах:** для MVP ограничены `Result<T, str>`. Расширение позже.
- **Zero values в Go:** для пользовательских struct'ов zero value — это struct с нулевыми полями, что может не быть валидным состоянием. Пока принимаем это как tradeoff.
- **Имена Go identifiers:** коллизия с Go-ключевыми словами (`type`, `func`, `go`, `chan`, etc.). Кодогенератор должен escape'ить пользовательские идентификаторы, конфликтующие с Go keywords, добавляя префикс `rgo_`.
- **Gofmt как runtime-зависимость:** компилятор зависит от наличия `gofmt` в PATH. Альтернатива — запасной in-process форматер (не для MVP).
- **`Self` в trait-параметрах (Go 1.26+ requirement):** полноценная поддержка `Self` опирается на self-referential type constraints, добавленные в Go 1.26 (февраль 2026). Это делает минимальную версию Go жёсткой: более ранние версии не смогут скомпилировать вывод для trait'ов с `Self`. В CI-проверках агент должен явно проверять версию через `go version` и отказывать при < 1.26.
- **Resolution метода при нескольких trait'ах:** если тип реализует два trait'а с методами одного имени, вызов `x.method()` неоднозначен. В MVP — простая реакция: ошибка "ambiguous method resolution" с рекомендацией использовать qualified-syntax; qualified-syntax (`Trait::method(&x)`) сам по себе в MVP **не** поддерживается, но ошибка корректно маркирует проблему для будущей фазы.
- **Default-методы и циклические зависимости:** default body может вызывать другие методы trait'а. В MVP считаем, что пользователь не создаёт бесконечную рекурсию; проверка — out of scope.

---

**Конец PRD v1.0.**
