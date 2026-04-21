// Generic enum nested payload: Option<T> payload preserves concrete
// instantiation through move/copy/clone/drop.

trait Drop {
  fn drop(&mut self);
}

trait Copy {}

trait Clone {
  fn clone(&self) -> Self;
}

// Outer wraps Option<T> — T must stay concrete through codegen
enum Outer<T> {
  Wrapped(Option<T>),
  Empty,
}

impl<T> Drop for Outer<T> {
  fn drop(&mut self) {
    println("drop-outer");
  }
}

// CopyOuter is Copy-eligible
enum CopyOuter<T> {
  Val(Option<T>),
}

impl<T> Copy for CopyOuter<T> {}

// Pair uses T twice — both must agree
enum Pair<T> {
  Both(T, T),
}

impl<T> Drop for Pair<T> {
  fn drop(&mut self) {
    println("drop-pair");
  }
}

fn main() {
  // Nested payload: Option<i64> preserves int64
  let a = Outer::Wrapped(Some(42));
  println("a-alive");

  // Move nested payload
  let b = Outer::Wrapped(Some(100));
  let c = b;
  println("b-moved");

  // Copy with nested payload
  let d = CopyOuter::Val(Some(99));
  let e = d;
  let f = d;
  println("d-copied");

  // Repeated tparam — both agree on i64
  let g = Pair::Both(10, 20);
  println("pair-alive");
}
