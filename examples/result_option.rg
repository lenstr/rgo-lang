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

fn demo_result() -> Result<i64, str> {
    let r = divide(10, 2)?;
    println(r);
    let h = half_divide(100, 4)?;
    println(h);
    Ok(0)
}

fn demo_option() -> Option<i64> {
    let f = first([1, 2, 3])?;
    println(f);
    let p = head_plus_one([10, 20])?;
    println(p);
    Some(p)
}

fn main() {
    demo_result();
    demo_option();
}
