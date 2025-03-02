package ast

import (
	"fmt"
	"windlang/lexer"
)

type UnaryOp int

const (
	UOPlus UnaryOp = iota
	UONegative
	UOBoolNot
	UOBinNot
	UORef
	UODeref

	UOCount
)

type UnaryOpNode struct {
	Expression Expression
	Op         UnaryOp
	Loc_       lexer.Location
}

var _ Expression = (*UnaryOpNode)(nil)

func (u UnaryOp) String() string {
	if UOCount != 6 {
		panic("Unary opperation enum length changed")
	}

	switch u {
	case UOPlus:
		return "+"
	case UONegative:
		return "-"
	case UOBoolNot:
		return "!"
	case UOBinNot:
		return "~"
	case UORef:
		return "&"
	case UODeref:
		return "*"
	}

	panic("not a unary op")
}

func (u UnaryOp) OnLeftSide() bool {
	if UOCount != 6 {
		panic("Unary opperation enum length changed")
	}

	switch u {
	case UOPlus, UONegative, UOBoolNot, UOBinNot, UORef, UODeref:
		return true
	}

	panic("not a unary op")
}

func (u UnaryOp) ReturnType(input Type) Type {
	if UOCount != 6 {
		panic("Unary opperation enum length changed")
	}

	switch u {
	case UOPlus:
		return input
	case UONegative:
		return input
	case UOBoolNot:
		return input
	case UOBinNot:
		return input
	case UORef:
		return PointerType{"", input}
	case UODeref:
		return input.(PointerType).Inner
	}

	panic("not a unary op")
}

func (u UnaryOpNode) string() string {
	if u.Op.OnLeftSide() {
		return fmt.Sprint(u.Op.String(), "(", u.Expression.string(), ")")
	}

	return fmt.Sprint("(", u.Expression.string(), ")", u.Op.String())
}

func (u UnaryOpNode) ReturnType() Type {
	return u.Op.ReturnType(u.Expression.ReturnType())
}

func (u UnaryOpNode) Loc() lexer.Location {
	return u.Loc_
}
