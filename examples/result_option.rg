fn divide(a: i64, b: i64) -> Result<i64, str> {
    if b == 0 {
        return Err("division by zero");
    }
    Ok(a / b)
}

fn double(s: str) -> Result<i64, str> {
    let n = parse_int(s)?;
    Ok(n * 2)
}

fn first(v: Vec<i64>) -> Option<i64> {
    if v.len() == 0 {
        return None;
    }
    Some(v[0])
}

fn main() {
    let r = divide(10, 2);
    let o = first([1, 2, 3]);
}
