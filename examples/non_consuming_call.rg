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
