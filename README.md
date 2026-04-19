# rgo

`rgo` — экспериментальный язык с синтаксисом в духе Rust и таргетом в Go.

Идея проекта: писать код в более выразительном, современном синтаксисе, а на выходе получать читаемый Go-код, который потом компилируется обычным `go build`.

- **Расширение файлов:** `.rg`
- **Имя CLI:** `rgoc`
- **Реализация компилятора:** OCaml 5.x
- **Таргет:** Go 1.26+

## Статус проекта

Сейчас репозиторий находится на стадии раннего прототипа.

На текущий момент уже есть:

- каркас проекта на `dune`
- CLI с `--version`
- Unicode-aware lexer на `sedlex`
- тесты на ключевые токены и позиции

Ниже в README приведён **целевой синтаксис языка**, то есть примеры того, как должен выглядеть `rgo` по мере развития компилятора.

## Быстрый старт

Все команды в репозитории нужно запускать через Nix flake:

```bash
nix develop -c dune build
nix develop -c dune runtest
nix develop -c dune exec rgoc -- --version
```

Ожидаемый вывод:

```text
rgoc v0.0.1
```

## Зачем нужен rgo

`rgo` пытается совместить:

- Rust-подобный синтаксис
- алгебраические типы (`enum`)
- `Option<T>` / `Result<T, E>`
- `match`
- `impl`, `trait`, generics
- оператор `?`

с практичным рантаймом Go:

- goroutines
- GC
- зрелая стандартная библиотека
- быстрый `go build`
- простая поставка бинарников

## Примеры синтаксиса

> Ниже — примеры целевого языка. Они описывают intended syntax проекта, даже если конкретная конструкция ещё не реализована в компиляторе.

### Hello world

```rust
fn main() {
    println("Hello, world!");
}
```

### Функции и `let`

```rust
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() {
    let x = add(20, 22);
    println(x);
}
```

### Изменяемые переменные

```rust
fn main() {
    let mut total: i64 = 0;
    total = total + 10;
    println(total);
}
```

### Условные выражения

```rust
fn abs(x: i64) -> i64 {
    if x < 0 {
        -x
    } else {
        x
    }
}
```

### Struct

```rust
struct Point {
    x: f64,
    y: f64,
}

fn distance_squared(a: Point, b: Point) -> f64 {
    let dx = a.x - b.x;
    let dy = a.y - b.y;
    dx * dx + dy * dy
}
```

### Enum и `match`

```rust
enum Shape {
    Circle(f64),
    Rectangle { width: f64, height: f64 },
    Empty,
}

fn area(shape: Shape) -> f64 {
    match shape {
        Shape::Circle(r) => 3.14 * r * r,
        Shape::Rectangle { width, height } => width * height,
        Shape::Empty => 0.0,
    }
}
```

### `Option<T>`

```rust
fn first(v: Vec<i64>) -> Option<i64> {
    if v.len() == 0 {
        return None;
    }

    Some(v[0])
}
```

### `Result<T, E>` и оператор `?`

```rust
fn parse_int(s: str) -> Result<i64, str> {
    // позже здесь будет биндинг к strconv.ParseInt
}

fn double(s: str) -> Result<i64, str> {
    let n = parse_int(s)?;
    Ok(n * 2)
}
```

### Методы через `impl`

```rust
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
```

### Ассоциированные функции

```rust
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    pub fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }
}

fn main() {
    let p = Point::new(1.0, 2.0);
    println(p.x);
}
```

### Trait и generic bounds

```rust
trait Display {
    fn display(&self) -> String;
}

struct User {
    name: String,
}

impl Display for User {
    fn display(&self) -> String {
        self.name
    }
}

fn print_one<T: Display>(value: T) {
    println(value.display());
}
```

### Коллекции

```rust
fn main() {
    let xs: Vec<i64> = [1, 2, 3, 4];
    let empty: Vec<String> = [];
    let zeros: Vec<i64> = [0; 8];

    println(xs.len());
    println(empty.len());
    println(zeros.len());
}
```

### Циклы

```rust
fn sum(xs: Vec<i64>) -> i64 {
    let mut total: i64 = 0;

    for x in xs {
        total = total + x;
    }

    total
}
```

### `while` и `loop`

```rust
fn countdown(mut n: i64) {
    while n > 0 {
        println(n);
        n = n - 1;
    }

    loop {
        break;
    }
}
```

## Пример будущего пайплайна

В целевом виде компилятор должен уметь делать примерно так:

```bash
rgoc examples/hello.rg -o hello.go
gofmt -w hello.go
go run hello.go
```

## Что уже проверяется тестами

Сейчас тесты покрывают в первую очередь lexer:

- ключевые слова (`fn`, `let`, `match`, `impl`, `trait` и т.д.)
- встроенные типы (`i64`, `String`, `Option`, `Result`, `Vec`, `HashMap`)
- строки, числа, операторы
- nested block comments
- Unicode identifiers
- позиции токенов

Запуск:

```bash
nix develop -c dune runtest
```

## Ближайшие шаги

Следующие крупные этапы проекта:

1. AST
2. parser на Menhir
3. name resolution
4. type checking
5. exhaustiveness check для `match`
6. codegen в Go

## Документация

Подробный продуктовый и технический план лежит здесь:

- `docs/PRD_rust_syntax_go_target_5.md`

Если хочешь быстро понять направление проекта — начни с этого README, а затем переходи к PRD.
