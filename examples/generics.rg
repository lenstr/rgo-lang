struct Box<T> {
    value: T,
}

impl<T> Box<T> {
    pub fn new(v: T) -> Self {
        Box { value: v }
    }

    pub fn get(&self) -> T {
        self.value
    }
}

fn first<T>(v: Vec<T>) -> Option<T> {
    if v.len() == 0 {
        return None;
    }
    Some(v[0])
}

fn main() {
    let b = Box::new(42);
    println(b.get());
}
