package main

import (
	"errors"
	"fmt"
)

func helper() (int64, error) {
	return 1, nil
}

func main() {
	a, __err_0 := func() (int64, error) {
		return 42, nil
	}()
	_ = __err_0
	_ = a

	if __err_0 == nil {
		v := a
		fmt.Println(v)
	} else {
		e := __err_0
		fmt.Println(e)
	}
	b, __err_1 := func() (int64, error) {
		if true {
			return 7, nil
		} else {
			return 0, errors.New("fail")
		}
	}()
	_ = __err_1
	_ = b

	if __err_1 == nil {
		v := b
		fmt.Println(v)
	} else {
		e := __err_1
		fmt.Println(e)
	}
	src, __err_2 := helper()
	_ = __err_2
	_ = src

	c, __err_3 := func() (int64, error) {
		if __err_2 == nil {
			return 99, nil
		} else {
			return 0, errors.New("nope")
		}
	}()
	_ = __err_3
	_ = c

	if __err_3 == nil {
		v := c
		fmt.Println(v)
	} else {
		e := __err_3
		fmt.Println(e)
	}
}
