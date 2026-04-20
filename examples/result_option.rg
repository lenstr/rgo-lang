fn divide(a: i64, b: i64) -> Result<i64, str> {
    if b == 0 {
        return Err("division by zero");
    }
    Ok(a / b)
}

fn half_divide(a: i64, b: i64) -> Result<i64, str> {
    let n = divide(a, b)?;
    Ok(n / 2)
}

fn first(v: Vec<i64>) -> Option<i64> {
    if v.len() == 0 {
        return None;
    }
    Some(v[0])
}

fn head_plus_one(v: Vec<i64>) -> Option<i64> {
    let n = first(v)?;
    Some(n + 1)
}

fn main() {
    show_divide();
    show_half_divide();
    show_first();
    show_head_plus_one();
}

fn show_divide() -> Result<i64, str> {
    let v = divide(10, 2)?;
    println(v);
    Ok(v)
}

fn show_half_divide() -> Result<i64, str> {
    let v = half_divide(100, 4)?;
    println(v);
    Ok(v)
}

fn show_first() -> Option<i64> {
    let v = first([1, 2, 3])?;
    println(v);
    Some(v)
}

fn show_head_plus_one() -> Option<i64> {
    let v = head_plus_one([10, 20])?;
    println(v);
    Some(v)
}
