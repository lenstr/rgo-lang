package main

import "fmt"

type Result[T any, E any] struct {
	ok    bool
	value T
	err   E
}

func nested_ok() Result[Result[int64, string], string] {
	return Result[Result[int64, string], string]{ok: true, value: Result[int64, string]{ok: true, value: 42}}
}

func nested_err() Result[Result[int64, string], string] {
	return Result[Result[int64, string], string]{err: "outer error"}
}

func inner_err() Result[Result[int64, string], string] {
	return Result[Result[int64, string], string]{ok: true, value: Result[int64, string]{err: "inner error"}}
}

func propagate() Result[Result[int64, string], string] {
	__res_0 := nested_ok()
	if !__res_0.ok {
		return Result[Result[int64, string], string]{err: __res_0.err}
	}
	x := __res_0.value
	_ = x

	return Result[Result[int64, string], string]{ok: true, value: x}
}

func propagate_err() Result[Result[int64, string], string] {
	__res_1 := nested_err()
	if !__res_1.ok {
		return Result[Result[int64, string], string]{err: __res_1.err}
	}
	x := __res_1.value
	_ = x

	return Result[Result[int64, string], string]{ok: true, value: x}
}

func main() {
	__match_res_2 := propagate()
	if __match_res_2.ok {
		if __match_res_2.value.ok {
			__inner_val_3 := __match_res_2.value.value
			v := __inner_val_3
			fmt.Println(v)
		} else {
			__inner_err_4 := __match_res_2.value.err
			e := __inner_err_4
			fmt.Println(e)
		}

	} else {
		e := __match_res_2.err
		fmt.Println(e)
	}
	__match_res_5 := propagate_err()
	if __match_res_5.ok {
		if __match_res_5.value.ok {
			__inner_val_6 := __match_res_5.value.value
			v := __inner_val_6
			fmt.Println(v)
		} else {
			__inner_err_7 := __match_res_5.value.err
			e := __inner_err_7
			fmt.Println(e)
		}

	} else {
		e := __match_res_5.err
		fmt.Println(e)
	}
	direct := Result[Result[int64, string], string]{ok: true, value: Result[int64, string]{ok: true, value: 7}}
	_ = direct

	if direct.ok {
		if direct.value.ok {
			__inner_val_8 := direct.value.value
			v := __inner_val_8
			fmt.Println(v)
		} else {
			__inner_err_9 := direct.value.err
			e := __inner_err_9
			fmt.Println(e)
		}

	} else {
		e := direct.err
		fmt.Println(e)
	}
	fmt.Println("done")
}
