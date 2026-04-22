package main

import (
	"errors"
	"fmt"
)

type Drop interface {
	Drop()
}

type Resource struct {
	Name string
}

func (self *Resource) Drop() {
	fmt.Println("drop:" + self.Name)
}

func ok_return() (int64, error) {
	a := Resource{Name: "ok-a"}
	_ = a

	__live_0 := true
	defer func() {
		if __live_0 {
			a.Drop()
		}
	}()
	_ = __live_0
	b := Resource{Name: "ok-b"}
	_ = b
	__live_1 := true

	fmt.Println("ok-before")

	if __live_1 {
		b.Drop()
	}
	__live_1 = false
	return 1, nil
}

func err_return() (int64, error) {
	a := Resource{Name: "err-a"}
	_ = a

	__live_2 := true
	defer func() {
		if __live_2 {
			a.Drop()
		}
	}()
	_ = __live_2
	b := Resource{Name: "err-b"}
	_ = b
	__live_3 := true

	fmt.Println("err-before")

	if __live_3 {
		b.Drop()
	}
	__live_3 = false
	return 0, errors.New("fail")
}

func some_return() *int64 {
	a := Resource{Name: "some-a"}
	_ = a

	__live_4 := true
	defer func() {
		if __live_4 {
			a.Drop()
		}
	}()
	_ = __live_4
	b := Resource{Name: "some-b"}
	_ = b
	__live_5 := true

	fmt.Println("some-before")

	if __live_5 {
		b.Drop()
	}
	__live_5 = false
	return new(int64(2))
}

func none_return() *int64 {
	a := Resource{Name: "none-a"}
	_ = a

	__live_6 := true
	defer func() {
		if __live_6 {
			a.Drop()
		}
	}()
	_ = __live_6
	b := Resource{Name: "none-b"}
	_ = b
	__live_7 := true

	fmt.Println("none-before")

	if __live_7 {
		b.Drop()
	}
	__live_7 = false
	return nil
}

func loop_break_test() {
	outer := Resource{Name: "break-outer"}
	_ = outer

	__live_8 := true
	defer func() {
		if __live_8 {
			outer.Drop()
		}
	}()
	_ = __live_8
	i := 0
	_ = i

	for i < 3 {
		a := Resource{Name: "break-body"}
		_ = a
		__live_9 := true

		if i == 1 {

			if __live_9 {
				a.Drop()
			}
			__live_9 = false
			break
		}
		fmt.Println("break-iter")
		i = i + 1
		if __live_9 {
			a.Drop()
		}
		__live_9 = false
	}
	fmt.Println("break-after")
}

func loop_continue_test() {
	outer := Resource{Name: "cont-outer"}
	_ = outer

	__live_10 := true
	defer func() {
		if __live_10 {
			outer.Drop()
		}
	}()
	_ = __live_10
	i := 0
	_ = i

	for i < 3 {
		a := Resource{Name: "cont-body"}
		_ = a
		__live_11 := true

		if i == 1 {
			i = i + 1

			if __live_11 {
				a.Drop()
			}
			__live_11 = false
			continue
		}
		fmt.Println("cont-iter")
		i = i + 1
		if __live_11 {
			a.Drop()
		}
		__live_11 = false
	}
	fmt.Println("cont-after")
}

func main() {
	_r1, __err_12 := ok_return()
	_ = __err_12
	_ = _r1

	fmt.Println("---")
	_r2, __err_13 := err_return()
	_ = __err_13
	_ = _r2

	fmt.Println("---")
	_r3 := some_return()
	_ = _r3

	fmt.Println("---")
	_r4 := none_return()
	_ = _r4

	fmt.Println("---")
	loop_break_test()
	fmt.Println("---")
	loop_continue_test()
}
