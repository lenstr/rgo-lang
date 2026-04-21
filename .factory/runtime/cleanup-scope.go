package main

import "fmt"

type Drop interface {
	Drop()
}

type Resource struct {
	Name string
}

func (self *Resource) Drop() {
	fmt.Println("drop:" + self.Name)
}

func take(r Resource) Resource {
	__live_0 := true
	defer func() {
		if __live_0 {
			r.Drop()
		}
	}()
	_ = __live_0
	__live_0 = false
	return r
}

func main() {
	fmt.Println("before-block")
	inner := Resource{Name: "inner"}
	_ = inner
	__live_1 := true

	fmt.Println("inside-block")
	if __live_1 {
		inner.Drop()
	}
	__live_1 = false

	fmt.Println("after-block")
	i := 0
	_ = i

	for i < 2 {
		iter := Resource{Name: "loop"}
		_ = iter
		__live_2 := true

		fmt.Println("iter")
		i = i + 1
		if __live_2 {
			iter.Drop()
		}
		__live_2 = false
	}
	fmt.Println("done-loop")
	x := Resource{Name: "original"}
	_ = x

	__live_3 := true
	defer func() {
		if __live_3 {
			x.Drop()
		}
	}()
	_ = __live_3
	y := take(x)
	_ = y
	__live_3 = false

	__live_4 := true
	defer func() {
		if __live_4 {
			y.Drop()
		}
	}()
	_ = __live_4
	fmt.Println("moved")
	r := take(Resource{Name: "wrapped"})
	_ = r

	__live_5 := true
	defer func() {
		if __live_5 {
			r.Drop()
		}
	}()
	_ = __live_5
	fmt.Println("caller:" + r.Name)
	alpha := Resource{Name: "alpha"}
	_ = alpha

	__live_6 := true
	defer func() {
		if __live_6 {
			alpha.Drop()
		}
	}()
	_ = __live_6
	fmt.Println(alpha.Name)
	fmt.Println("still-alive")
}
