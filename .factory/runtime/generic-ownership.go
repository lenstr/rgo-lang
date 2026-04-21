package main

import "fmt"

type Drop interface {
	Drop()
}

type Clone[Self any] interface {
	Clone() Self
}

type Copy interface {
}

type Container[T any] struct {
	Value T
}

func ContainerNew[T any](v T) Container[T] {
	return Container[T]{Value: v}
}

func (self *Container[T]) Drop() {
	fmt.Println("drop-container")
}

func (self Container[T]) Clone() Container[T] {
	return Container[T]{Value: self.Value}
}

type CopyBox[T any] struct {
	Value T
}

func CopyBoxNew[T any](v T) CopyBox[T] {
	return CopyBox[T]{Value: v}
}

func consume[T any](c Container[T]) {
	__live_0 := true
	defer func() {
		if __live_0 {
			c.Drop()
		}
	}()
	_ = __live_0
	fmt.Println("consumed")
}

func use_copybox[T any](b CopyBox[T]) {
	fmt.Println("used-copybox")
}

func main() {
	a := ContainerNew(42)
	_ = a
	__live_1 := true
	defer func() {
		if __live_1 {
			a.Drop()
		}
	}()
	_ = __live_1
	b := a
	_ = b
	__live_1 = false
	__live_2 := true
	defer func() {
		if __live_2 {
			b.Drop()
		}
	}()
	_ = __live_2
	fmt.Println("moved")
	c := ContainerNew("hello")
	_ = c
	__live_3 := true
	defer func() {
		if __live_3 {
			c.Drop()
		}
	}()
	_ = __live_3
	consume(c.Clone())
	fmt.Println("still-alive-after-clone")
	d := CopyBoxNew(99)
	_ = d
	use_copybox(d)
	use_copybox(d)
	fmt.Println("copy-reusable")
}
