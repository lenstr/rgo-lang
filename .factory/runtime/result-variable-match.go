package main

import (
	"errors"
	"fmt"
)

func get_result(x int64) (int64, error) {
	if x > 0 {
		return x, nil
	} else {
		return 0, errors.New("negative")
	}
}

func main() {
	r, __err_0 := get_result(42)
	_ = __err_0
	_ = r

	if __err_0 == nil {
		v := r
		fmt.Println(v)
	} else {
		e := __err_0
		fmt.Println(e)
	}
}
