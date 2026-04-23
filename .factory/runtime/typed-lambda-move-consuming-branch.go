package main

import "fmt"

type Drop interface {
	Drop()
}

type Wrapper struct {
	value int32
}

func (self *Wrapper) Drop() {
}

func take(w Wrapper) int32 {
	__live_0 := true
	defer func() {
		if __live_0 {
			w.Drop()
		}
	}()
	_ = __live_0
	return w.value
}

func main() {
	f := func(a int32) int32 {
		w := Wrapper{value: 42}
		_ = w

		__live_1 := true
		defer func() {
			if __live_1 {
				w.Drop()
			}
		}()
		_ = __live_1
		if a > 0 {
			return a
		} else {
			__live_1 = false
			return take(w)
		}
	}
	_ = f

	fmt.Println(f(5))
	fmt.Println(f(-1))
}
