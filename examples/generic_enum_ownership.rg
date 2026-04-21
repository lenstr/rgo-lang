// Generic enum ownership: direct variant constructors with Drop/Copy/move.

trait Drop {
  fn drop(&mut self);
}

trait Copy {}

enum Wrapper<T> {
  Some(T),
  None,
}

impl<T> Drop for Wrapper<T> {
  fn drop(&mut self) {
    println("drop-wrapper");
  }
}

enum CopyEnum<T> {
  Val(T),
}

impl<T> Copy for CopyEnum<T> {}

fn main() {
  // Generic enum variant constructor with Drop — cleanup fires once
  let a = Wrapper::Some(42);
  println("a-alive");

  // Move: moved value has cleanup transferred
  let b = Wrapper::Some("hello");
  let c = b;
  println("b-moved");

  // Copy: copies keep original alive
  let d = CopyEnum::Val(99);
  let e = d;
  let f = d;
  println("d-copied");

  // Two distinct instantiations
  let g = Wrapper::Some(100);
  let h = Wrapper::Some("world");
  println("two-instantiations");
}
