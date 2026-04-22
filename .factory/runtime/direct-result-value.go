package main

import "fmt"

type Result[T any, E any] struct {
	ok    bool
	value T
	err   E
}

func main() {
	r := Result[int64, string]{ok: true, value: 42}
	_ = r

	if r.ok {
		v := r.value
		fmt.Println(v)
	} else {
		e := r.err
		fmt.Println(e)
	}
	__match_res_0 := Result[int64, string]{err: "hello"}
	if __match_res_0.ok {
		v := __match_res_0.value
		fmt.Println(v)
	} else {
		e := __match_res_0.err
		fmt.Println(e)
	}
	nested := Result[Result[int64, string], string]{ok: true, value: Result[int64, string]{ok: true, value: 7}}
	_ = nested

	if nested.ok {
		if nested.value.ok {
			__inner_val_1 := nested.value.value
			v := __inner_val_1
			fmt.Println(v)
		} else {
			__inner_err_2 := nested.value.err
			e := __inner_err_2
			fmt.Println(e)
		}

	} else {
		e := nested.err
		fmt.Println(e)
	}
	inner := Result[int64, string]{ok: true, value: 99}
	_ = inner

	if inner.ok {
		v := inner.value
		fmt.Println(v)
	} else {
		e := inner.err
		fmt.Println(e)
	}
}
