fn main() {
    let mut sum: i64 = 0;
    let xs: Vec<i64> = [1, 2, 3, 4, 5];

    for x in xs {
        sum = sum + x;
    }

    let mut i: i64 = 0;
    while i < 10 {
        i = i + 1;
    }

    println(sum);
    println(i);
}
