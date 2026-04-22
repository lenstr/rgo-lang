// VAL-OWN-006 runtime fixture: final-expression specialized Ok/Err/Some/None returns
// perform the same nested cleanup and consumed-guard suppression as explicit returns.

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

// Final Ok(r) suppresses r's guard and cleans up other bindings
fn final_ok_drop() -> Result<Resource, str> {
  let other = Resource { name: "other" };
  let r = Resource { name: "r" };
  println("before-ok");
  Ok(r)
}

// Final Err suppresses guard for other bindings
fn final_err_drop() -> Result<i64, str> {
  let other = Resource { name: "other2" };
  println("before-err");
  Err("fail")
}

// Final Some(r) suppresses r's guard and cleans up other bindings
fn final_some_drop() -> Option<Resource> {
  let other = Resource { name: "other3" };
  let r = Resource { name: "r3" };
  println("before-some");
  Some(r)
}

// Final None cleans up all Drop bindings
fn final_none_drop() -> Option<i64> {
  let other = Resource { name: "other4" };
  println("before-none");
  None
}

// Explicit return Ok(r) suppresses r's guard (comparison baseline)
fn explicit_ok_drop() -> Result<Resource, str> {
  let other = Resource { name: "other5" };
  let r = Resource { name: "r5" };
  println("before-explicit-ok");
  return Ok(r);
}

fn main() {
  let r1 = final_ok_drop();
  println("---");
  let r2 = final_err_drop();
  println("---");
  let r3 = final_some_drop();
  println("---");
  let r4 = final_none_drop();
  println("---");
  let r5 = explicit_ok_drop();
  println("done");
}
