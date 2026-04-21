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
  // VAL-OWN-005: nested block cleanup fires at block exit
  println("before-block");
  {
    let inner = Resource { name: "inner" };
    println("inside-block");
  }
  println("after-block");

  // VAL-OWN-005: loop body cleanup each iteration
  let mut i = 0;
  while i < 2 {
    let iter = Resource { name: "loop" };
    println("iter");
    i = i + 1;
  }
  println("done-loop");

  // Consuming move in let init suppresses guard exactly once
  let x = Resource { name: "original" };
  let y = take(x);
  println("moved");

  // Consuming move in return expression suppresses guard exactly once
  let r = take(Resource { name: "wrapped" });
  println("caller:" + r.name);

  // Imported stdlib non-consuming call does not suppress Drop cleanup
  let alpha = Resource { name: "alpha" };
  println(alpha.name);
  println("still-alive");
}
