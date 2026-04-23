trait Drop {
    fn drop(&mut self);
}

struct Wrapper {
    value: i32,
}

impl Drop for Wrapper {
    fn drop(&mut self) {}
}

fn take(w: Wrapper) -> i32 {
    w.value
}

fn main() {
    let f = |a: i32| -> i32 {
        let w = Wrapper { value: 42 };
        if a > 0 {
            return a;
        } else {
            take(w)
        }
    };
    println(f(5));
    println(f(-1));
}
