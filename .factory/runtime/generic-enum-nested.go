package main

import "fmt"

type Drop interface {
	Drop()
}

type Copy interface {
}

type Clone[Self any] interface {
	Clone() Self
}

type Outer[T any] interface {
	isOuter()
	Drop()
}

type OuterWrapped[T any] struct {
	Field0 *T
}

func (OuterWrapped[T]) isOuter() {}

type OuterEmpty[T any] struct{}

func (OuterEmpty[T]) isOuter() {}

func outerDropImpl[T any](self Outer[T]) {
	fmt.Println("drop-outer")
}

func (self OuterWrapped[T]) Drop() {
	outerDropImpl[T](self)
}

func (self OuterEmpty[T]) Drop() {
	outerDropImpl[T](self)
}

type CopyOuter[T any] interface {
	isCopyOuter()
}

type CopyOuterVal[T any] struct {
	Field0 *T
}

func (CopyOuterVal[T]) isCopyOuter() {}

type Pair[T any] interface {
	isPair()
	Drop()
}

type PairBoth[T any] struct {
	Field0 T
	Field1 T
}

func (PairBoth[T]) isPair() {}

func pairDropImpl[T any](self Pair[T]) {
	fmt.Println("drop-pair")
}

func (self PairBoth[T]) Drop() {
	pairDropImpl[T](self)
}

func main() {
	var a Outer[int64] = OuterWrapped[int64]{Field0: new(int64(42))}
	_ = a
	__live_0 := true
	defer func() {
		if __live_0 {
			a.Drop()
		}
	}()
	_ = __live_0
	fmt.Println("a-alive")
	var b Outer[int64] = OuterWrapped[int64]{Field0: new(int64(100))}
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
	var d CopyOuter[int64] = CopyOuterVal[int64]{Field0: new(int64(99))}
	_ = d
	e := d
	_ = e
	f := d
	_ = f
	fmt.Println("d-copied")
	var g Pair[int64] = PairBoth[int64]{Field0: 10, Field1: 20}
	_ = g
	__live_3 := true
	defer func() {
		if __live_3 {
			g.Drop()
		}
	}()
	_ = __live_3
	fmt.Println("pair-alive")
}
