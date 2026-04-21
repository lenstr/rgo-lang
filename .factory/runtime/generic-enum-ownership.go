package main

import "fmt"

type Drop interface {
	Drop()
}

type Copy interface {
}

type Wrapper[T any] interface {
	isWrapper()
	Drop()
}

type WrapperSome[T any] struct {
	Field0 T
}

func (WrapperSome[T]) isWrapper() {}

type WrapperNone struct{}

func (WrapperNone) isWrapper() {}

func wrapperDropImpl[T any](self Wrapper[T]) {
	fmt.Println("drop-wrapper")
}

func (self WrapperSome[T]) Drop() {
	wrapperDropImpl[T](self)
}

func (self WrapperNone) Drop() {
	wrapperDropImpl[any](self)
}

type CopyEnum[T any] interface {
	isCopyEnum()
}

type CopyEnumVal[T any] struct {
	Field0 T
}

func (CopyEnumVal[T]) isCopyEnum() {}

func main() {
	var a Wrapper[int64] = WrapperSome[int64]{Field0: 42}
	_ = a
	__live_0 := true
	defer func() {
		if __live_0 {
			a.Drop()
		}
	}()
	_ = __live_0
	fmt.Println("a-alive")
	var b Wrapper[string] = WrapperSome[string]{Field0: "hello"}
	_ = b
	__live_1 := true
	defer func() {
		if __live_1 {
			b.Drop()
		}
	}()
	_ = __live_1
	c := b
	_ = c
	__live_1 = false
	__live_2 := true
	defer func() {
		if __live_2 {
			c.Drop()
		}
	}()
	_ = __live_2
	fmt.Println("b-moved")
	var d CopyEnum[int64] = CopyEnumVal[int64]{Field0: 99}
	_ = d
	e := d
	_ = e
	f := d
	_ = f
	fmt.Println("d-copied")
	var g Wrapper[int64] = WrapperSome[int64]{Field0: 100}
	_ = g
	__live_3 := true
	defer func() {
		if __live_3 {
			g.Drop()
		}
	}()
	_ = __live_3
	var h Wrapper[string] = WrapperSome[string]{Field0: "world"}
	_ = h
	__live_4 := true
	defer func() {
		if __live_4 {
			h.Drop()
		}
	}()
	_ = __live_4
	fmt.Println("two-instantiations")
}
