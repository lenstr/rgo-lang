fn nested_result(x: i64) -> Result<Result<i64, str>, str> {
    if x > 0 {
        Ok(Ok(x) as Result<i64, str>)
    } else if x == 0 {
        Ok(Err("zero") as Result<i64, str>)
    } else {
        Err("negative")
    }
}

fn main() {
    match nested_result(42) {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
    match nested_result(0) {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
    match nested_result(-1) {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
}
