// Generic enum Clone: clone-based reuse paths with Drop.

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
