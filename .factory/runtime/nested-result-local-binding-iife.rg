enum MyOpt {
    Some(i64),
    None,
}

fn test_if_local_binding() -> Result<i64, str> {
    let inner = if true {
        let x = Ok(7);
        x
    } else {
        let y = Err("fail");
        y
    };
    match inner {
        Result::Ok(v) => Ok(v),
        Result::Err(e) => Err(e),
    }
}

fn test_match_local_binding() -> Result<Result<i64, str>, str> {
    let src: Result<Result<i64, str>, str> = Ok(Ok(42));
    let inner = match src {
        Result::Ok(v) => {
            let x = Ok(v);
            x
        },
        Result::Err(e) => {
            let y = Err(e);
            y
        },
    };
    inner
}

fn test_match_pattern_binding() -> i64 {
    let src: MyOpt = MyOpt::Some(99);
    let inner = match src {
        MyOpt::Some(v) => v,
        MyOpt::None => 0,
    };
    inner
}

fn main() {
    match test_if_local_binding() {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    };
    match test_match_local_binding() {
        Result::Ok(v) => println(v),
        Result::Err(e) => println(e),
    };
    println(test_match_pattern_binding());
}
