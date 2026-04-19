trait Display {
    fn display(&self) -> String;
}

trait Summary {
    fn summary(&self) -> String;
    fn short(&self) -> String {
        self.summary()
    }
}

struct Article {
    pub title: String,
    pub body: String,
}

impl Display for Article {
    fn display(&self) -> String {
        self.title + ": " + self.body
    }
}

impl Summary for Article {
    fn summary(&self) -> String {
        self.title
    }
}

fn print_all<T: Display + Summary>(items: Vec<T>) {
    for item in items {
        println(item.summary());
        println(item.display());
    }
}

fn main() {
    let a = Article { title: "Hello", body: "World" };
    println(a.display());
    println(a.short());
}
