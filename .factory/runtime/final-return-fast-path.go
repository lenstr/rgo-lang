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

func final_ok_drop() (Resource, error) {
	other := Resource{Name: "other"}
	_ = other

	__live_0 := true
	defer func() {
		if __live_0 {
			other.Drop()
		}
	}()
	_ = __live_0
	r := Resource{Name: "r"}
	_ = r

	__live_1 := true
	defer func() {
		if __live_1 {
			r.Drop()
		}
	}()
	_ = __live_1
	fmt.Println("before-ok")
	__live_1 = false
	return r, nil
}

func final_err_drop() (int64, error) {
	other := Resource{Name: "other2"}
	_ = other

	__live_2 := true
	defer func() {
		if __live_2 {
			other.Drop()
		}
	}()
	_ = __live_2
	fmt.Println("before-err")
	return 0, errors.New("fail")
}

func final_some_drop() *Resource {
	other := Resource{Name: "other3"}
	_ = other

	__live_3 := true
	defer func() {
		if __live_3 {
			other.Drop()
		}
	}()
	_ = __live_3
	r := Resource{Name: "r3"}
	_ = r

	__live_4 := true
	defer func() {
		if __live_4 {
			r.Drop()
		}
	}()
	_ = __live_4
	fmt.Println("before-some")
	__live_4 = false
	return new(r)
}

func final_none_drop() *int64 {
	other := Resource{Name: "other4"}
	_ = other

	__live_5 := true
	defer func() {
		if __live_5 {
			other.Drop()
		}
	}()
	_ = __live_5
	fmt.Println("before-none")
	return nil
}

func explicit_ok_drop() (Resource, error) {
	other := Resource{Name: "other5"}
	_ = other

	__live_6 := true
	defer func() {
		if __live_6 {
			other.Drop()
		}
	}()
	_ = __live_6
	r := Resource{Name: "r5"}
	_ = r

	__live_7 := true
	defer func() {
		if __live_7 {
			r.Drop()
		}
	}()
	_ = __live_7
	fmt.Println("before-explicit-ok")
	__live_7 = false
	return r, nil
}

func main() {
	r1, __err_8 := final_ok_drop()
	_ = __err_8
	_ = r1

	fmt.Println("---")
	r2, __err_9 := final_err_drop()
	_ = __err_9
	_ = r2

	fmt.Println("---")
	r3 := final_some_drop()
	_ = r3

	fmt.Println("---")
	r4 := final_none_drop()
	_ = r4

	fmt.Println("---")
	r5, __err_10 := explicit_ok_drop()
	_ = __err_10
	_ = r5

	fmt.Println("done")
}
