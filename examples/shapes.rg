enum Shape {
    Circle(f64),
    Rectangle { width: f64, height: f64 },
    Empty,
}

fn area(s: Shape) -> f64 {
    match s {
        Shape::Circle(r) => 3.14 * r * r,
        Shape::Rectangle { width, height } => width * height,
        Shape::Empty => 0.0,
    }
}

fn main() {
    let c = Shape::Circle(5.0);
    println(area(c));
}
