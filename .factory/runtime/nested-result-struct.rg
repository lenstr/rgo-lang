fn nested_ok() -> Result<Result<i64, str>, str> {
    Ok(Ok(42) as Result<i64, str>)
}

fn nested_err() -> Result<Result<i64, str>, str> {
    Err("outer error")
}

fn inner_err() -> Result<Result<i64, str>, str> {
    Ok(Err("inner error") as Result<i64, str>)
}

fn propagate() -> Result<Result<i64, str>, str> {
    let x = nested_ok()?;
    Ok(x)
}

fn propagate_err() -> Result<Result<i64, str>, str> {
    let x = nested_err()?;
    Ok(x)
}

fn main() {
    match propagate() {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
    match propagate_err() {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
    let direct = Ok(Ok(7) as Result<i64, str>);
    match direct {
        Result::Ok(Result::Ok(v)) => println(v),
        Result::Ok(Result::Err(e)) => println(e),
        Result::Err(e) => println(e),
    };
    println("done");
}
