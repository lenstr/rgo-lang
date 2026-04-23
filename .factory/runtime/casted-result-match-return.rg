fn wrap_ok(x: i64) -> Result<i64, str> {
    match Ok(x) as Result<i64, str> {
        Result::Ok(v) => Ok(v + 1) as Result<i64, str>,
        Result::Err(e) => Err(e) as Result<i64, str>,
    }
}

fn wrap_err() -> Result<i64, str> {
    match Err("bad") as Result<i64, str> {
        Result::Ok(v) => Ok(v) as Result<i64, str>,
        Result::Err(e) => Err("wrapped: " + e) as Result<i64, str>,
    }
}

fn double(r: Result<i64, str>) -> Result<i64, str> {
    match r {
        Result::Ok(v) => Ok(v * 2) as Result<i64, str>,
        Result::Err(e) => Err("var: " + e) as Result<i64, str>,
    }
}

fn main() {
    let a = wrap_ok(41);
    let b = wrap_err();
    let c = double(Ok(5));
    let d = double(Err("fail"));

    match a {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    };
    match b {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    };
    match c {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    };
    match d {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    };
}
