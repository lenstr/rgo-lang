// Generic ownership: move/copy/clone/drop preserve concrete instantiations.

trait Drop {
  fn drop(&mut self);
}

trait Clone {
  fn clone(&self) -> Self;
}

trait Copy {}

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

impl<T> Clone for Container<T> {
  fn clone(&self) -> Self {
    Container { value: self.value }
  }
}

struct CopyBox<T> {
  pub value: T,
}

impl<T> CopyBox<T> {
  pub fn new(v: T) -> Self {
    CopyBox { value: v }
  }
}

impl<T> Copy for CopyBox<T> {}

fn consume<T>(c: Container<T>) {
  println("consumed");
}

fn use_copybox<T>(b: CopyBox<T>) {
  println("used-copybox");
}

fn main() {
  // Move: non-Copy generic struct transfers ownership
  let a = Container::new(42);
  let b = a;
  // a is now moved; only b is live
  println("moved");

  // Clone: explicit duplication at boundary
  let c = Container::new("hello");
  consume(c.clone());
  println("still-alive-after-clone");

  // Copy: Copy generic struct stays usable
  let d = CopyBox::new(99);
  use_copybox(d);
  use_copybox(d);
  println("copy-reusable");

  // Drop: cleanup fires for live bindings
  // b: Container<i64> - live, will drop
  // c: Container<str> - live, will drop
  // d: CopyBox<i64> - not Drop, no cleanup
}
