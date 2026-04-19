struct Counter {
    value: i64,
}

impl Counter {
    pub fn new() -> Self {
        Counter { value: 0 }
    }

    pub fn get(&self) -> i64 {
        self.value
    }

    pub fn inc(&mut self) {
        self.value = self.value + 1;
    }
}

fn main() {
    let mut c = Counter::new();
    c.inc();
    c.inc();
    println(c.get());
}
