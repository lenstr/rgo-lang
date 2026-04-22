fn main() {
    // Direct let-binding with Result constructor
    let r: Result<i64, str> = Ok(42);
    match r {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }

    // Direct match scrutinee with Result constructor
    match Err("hello") {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }

    // Nested Result constructor in let-binding
    let nested: Result<Result<i64, str>, str> = Ok(Ok(7));
    match nested {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    }

    // Variable holding Result struct matched later
    let inner: Result<i64, str> = Ok(99);
    match inner {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    }
}
