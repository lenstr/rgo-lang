fn maybe_value(x: i64) -> Result<Option<Result<i64, str>>, str> {
    if x > 0 {
        Ok(Some(Ok(x) as Result<i64, str>))
    } else if x == 0 {
        Ok(Some(Err("zero") as Result<i64, str>))
    } else {
        Err("negative")
    }
}

fn main() {
    match maybe_value(42) {
        Result::Ok(Option::Some(Result::Ok(v))) => println(v),
        Result::Ok(Option::Some(Result::Err(e))) => println(e),
        Result::Ok(Option::None) => println("none"),
        Result::Err(e) => println(e),
    };
    match maybe_value(0) {
        Result::Ok(Option::Some(Result::Ok(v))) => println(v),
        Result::Ok(Option::Some(Result::Err(e))) => println(e),
        Result::Ok(Option::None) => println("none"),
        Result::Err(e) => println(e),
    };
    match maybe_value(-1) {
        Result::Ok(Option::Some(Result::Ok(v))) => println(v),
        Result::Ok(Option::Some(Result::Err(e))) => println(e),
        Result::Ok(Option::None) => println("none"),
        Result::Err(e) => println(e),
    };
}
