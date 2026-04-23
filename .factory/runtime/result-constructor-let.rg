fn helper() -> Result<i64, str> {
    Ok(1)
}

fn main() {
    let a = { Ok(42) };
    match a {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }

    let b = if true { Ok(7) } else { Err("fail") };
    match b {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }

    let src = helper();
    let c = match src {
        Result::Ok(_) => Ok(99),
        Result::Err(_) => Err("nope"),
    };
    match c {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
