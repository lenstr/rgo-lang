fn main() {
    let r1 = Ok(42) as Result<i64, str>;
    match r1 {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }

    match Ok(99) as Result<i64, str> {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }

    match Err("hello") as Result<i64, str> {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }

    let r2 = Err("fail") as Result<i64, str>;
    match r2 {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
