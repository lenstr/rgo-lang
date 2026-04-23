package main

import "fmt"

type Result[T any, E any] struct {
	ok    bool
	value T
	err   E
}

func nested_result(x int64) Result[Result[int64, string], string] {
	if x > 0 {
		return Result[Result[int64, string], string]{ok: true, value: Result[int64, string]{ok: true, value: x}}
	} else {
		if x == 0 {
			return Result[Result[int64, string], string]{ok: true, value: Result[int64, string]{err: "zero"}}
		} else {
			return Result[Result[int64, string], string]{err: "negative"}
		}
	}
}

func main() {
	__match_res_0 := nested_result(42)
	if __match_res_0.ok {
		if __match_res_0.value.ok {
			__inner_val_1 := __match_res_0.value.value
			v := __inner_val_1
			fmt.Println(v)
		} else {
			__inner_err_2 := __match_res_0.value.err
			e := __inner_err_2
			fmt.Println(e)
		}

	} else {
		e := __match_res_0.err
		fmt.Println(e)
	}
	__match_res_3 := nested_result(0)
	if __match_res_3.ok {
		if __match_res_3.value.ok {
			__inner_val_4 := __match_res_3.value.value
			v := __inner_val_4
			fmt.Println(v)
		} else {
			__inner_err_5 := __match_res_3.value.err
			e := __inner_err_5
			fmt.Println(e)
		}

	} else {
		e := __match_res_3.err
		fmt.Println(e)
	}
	__match_res_6 := nested_result(-1)
	if __match_res_6.ok {
		if __match_res_6.value.ok {
			__inner_val_7 := __match_res_6.value.value
			v := __inner_val_7
			fmt.Println(v)
		} else {
			__inner_err_8 := __match_res_6.value.err
			e := __inner_err_8
			fmt.Println(e)
		}

	} else {
		e := __match_res_6.err
		fmt.Println(e)
	}
}
