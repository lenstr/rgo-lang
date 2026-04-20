enum Color {
    Red,
    Green,
    Blue,
}
fn describe(c: Color) -> str {
    match c {
        Color::Red => "red",
        Color::Green => "green",
    }
}
fn main() {
    let s = describe(Color::Red);
}
