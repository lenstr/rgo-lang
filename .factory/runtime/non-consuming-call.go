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

func consume(r Resource) {
	__live_0 := true
	defer func() {
		if __live_0 {
			r.Drop()
		}
	}()
	_ = __live_0
	fmt.Println("consumed:" + r.Name)
}

func main() {
	a := Resource{Name: "kept"}
	_ = a
	__live_1 := true
	defer func() {
		if __live_1 {
			a.Drop()
		}
	}()
	_ = __live_1
	b := Resource{Name: "given"}
	_ = b
	__live_2 := true
	defer func() {
		if __live_2 {
			b.Drop()
		}
	}()
	_ = __live_2
	fmt.Println(a.Name)
	consume(b)
	__live_2 = false
	fmt.Println(a.Name)
	fmt.Println("end")
}
