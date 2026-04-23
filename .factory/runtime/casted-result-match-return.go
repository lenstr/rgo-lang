package main

import (
	"errors"
	"fmt"
)

type Result[T any, E any] struct {
	ok    bool
	value T
	err   E
}

func wrap_ok(x int64) (int64, error) {
	__match_res_0 := Result[int64, string]{ok: true, value: x}
	if __match_res_0.ok {
		v := __match_res_0.value
		return v + 1, nil
	} else {
		e := __match_res_0.err
		return 0, errors.New(e)
	}
}

func wrap_err() (int64, error) {
	__match_res_1 := Result[int64, string]{err: "bad"}
	if __match_res_1.ok {
		v := __match_res_1.value
		return v, nil
	} else {
		e := __match_res_1.err
		return 0, errors.New("wrapped: " + e)
	}
}

func double(r Result[int64, string]) (int64, error) {
	if r.ok {
		v := r.value
		return v * 2, nil
	} else {
		e := r.err
		return 0, errors.New("var: " + e)
	}
}

func main() {
	a, __err_2 := wrap_ok(41)
	_ = __err_2
	_ = a

	b, __err_3 := wrap_err()
	_ = __err_3
	_ = b

	c, __err_4 := double(Result[int64, string]{ok: true, value: 5})
	_ = __err_4
	_ = c

	d, __err_5 := double(Result[int64, string]{err: "fail"})
	_ = __err_5
	_ = d

	if __err_2 == nil {
		v := a
		fmt.Println(v)
	} else {
		e := __err_2
		fmt.Println(e)
	}
	if __err_3 == nil {
		v := b
		fmt.Println(v)
	} else {
		e := __err_3
		fmt.Println(e)
	}
	if __err_4 == nil {
		v := c
		fmt.Println(v)
	} else {
		e := __err_4
		fmt.Println(e)
	}
	if __err_5 == nil {
		v := d
		fmt.Println(v)
	} else {
		e := __err_5
		fmt.Println(e)
	}
}
