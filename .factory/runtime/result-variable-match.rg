fn get_result(x: i64) -> Result<i64, str> {
    if x > 0 {
        Ok(x)
    } else {
        Err("negative")
    }
}

fn main() {
    let r = get_result(42);
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
