enum Color {
    Red,
    Green,
    Blue,
}
fn describe(c: Color) -> str {
    match c {
        Color::Red => "red",
        Color::Green => "green",
        Color::Blue => "blue",
    }
}
fn main() {
    let s = describe(Color::Red);
}
