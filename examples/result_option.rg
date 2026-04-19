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
    let r = divide(10, 2);
    let h = half_divide(100, 4);
    let o = first([1, 2, 3]);
    let p = head_plus_one([10, 20]);
}
