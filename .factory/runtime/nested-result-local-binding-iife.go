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

type MyOpt interface {
	isMyOpt()
}

type MyOptSome struct {
	Field0 int64
}

func (MyOptSome) isMyOpt() {}

type MyOptNone struct{}

func (MyOptNone) isMyOpt() {}

func test_if_local_binding() (int64, error) {
	inner := func() Result[int64, string] {
		if true {
			x := Result[int64, string]{ok: true, value: 7}
			_ = x

			return x
		} else {
			y := Result[int64, string]{err: "fail"}
			_ = y

			return y
		}
	}()
	_ = inner

	if inner.ok {
		v := inner.value
		return v, nil
	} else {
		e := inner.err
		return 0, errors.New(e)
	}
}

func test_match_local_binding() Result[Result[int64, string], string] {
	src := Result[Result[int64, string], string]{ok: true, value: Result[int64, string]{ok: true, value: 42}}
	_ = src

	inner := func() Result[Result[int64, string], string] {
		if src.ok {
			v := src.value
			return func() Result[Result[int64, string], string] {
				x := Result[Result[int64, string], string]{ok: true, value: v}
				_ = x

				return x
			}()
		} else {
			e := src.err
			return func() Result[Result[int64, string], string] {
				y := Result[Result[int64, string], string]{err: e}
				_ = y

				return y
			}()
		}
	}()
	_ = inner

	return inner
}

func test_match_pattern_binding() int64 {
	var src MyOpt = MyOptSome{Field0: 99}
	_ = src

	inner := func() int64 {
		switch __v := src.(type) {
		case MyOptSome:
			v := __v.Field0
			return v
		case MyOptNone:
			_ = __v
			return 0
		default:
			panic("unreachable: non-exhaustive match")
		}
	}()
	_ = inner

	return inner
}

func main() {
	if __match_val_0, __match_err_1 := test_if_local_binding(); __match_err_1 == nil {
		v := __match_val_0
		fmt.Println(v)
	} else {
		e := __match_err_1
		fmt.Println(e)
	}
	__match_res_2 := test_match_local_binding()
	if __match_res_2.ok {
		v := __match_res_2.value
		fmt.Println(v)
	} else {
		e := __match_res_2.err
		fmt.Println(e)
	}
	fmt.Println(test_match_pattern_binding())
}
