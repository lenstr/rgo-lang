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

fn ok_return() -> Result<i64, str> {
  let a = Resource { name: "ok-a" };
  {
    let b = Resource { name: "ok-b" };
    println("ok-before");
    return Ok(1);
  }
}

fn err_return() -> Result<i64, str> {
  let a = Resource { name: "err-a" };
  {
    let b = Resource { name: "err-b" };
    println("err-before");
    return Err("fail");
  }
}

fn some_return() -> Option<i64> {
  let a = Resource { name: "some-a" };
  {
    let b = Resource { name: "some-b" };
    println("some-before");
    return Some(2);
  }
}

fn none_return() -> Option<i64> {
  let a = Resource { name: "none-a" };
  {
    let b = Resource { name: "none-b" };
    println("none-before");
    return None;
  }
}

fn loop_break_test() {
  let outer = Resource { name: "break-outer" };
  let mut i = 0;
  while i < 3 {
    let a = Resource { name: "break-body" };
    if i == 1 {
      break;
    }
    println("break-iter");
    i = i + 1;
  }
  println("break-after");
}

fn loop_continue_test() {
  let outer = Resource { name: "cont-outer" };
  let mut i = 0;
  while i < 3 {
    let a = Resource { name: "cont-body" };
    if i == 1 {
      i = i + 1;
      continue;
    }
    println("cont-iter");
    i = i + 1;
  }
  println("cont-after");
}

fn main() {
  let _r1 = ok_return();
  println("---");
  let _r2 = err_return();
  println("---");
  let _r3 = some_return();
  println("---");
  let _r4 = none_return();
  println("---");
  loop_break_test();
  println("---");
  loop_continue_test();
}
