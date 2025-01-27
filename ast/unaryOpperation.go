package ast

import (
	"errors"
	"fmt"
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

func (u UnaryOp) InputAllowed(input Kind) bool {
	if UOCount != 6 {
		panic("Unary opperation enum length changed")
	}

	switch u {
	case UOPlus:
		return input&KindNumberMask != 0
	case UONegative:
		return input&KindNumberMask != 0
	case UOBoolNot:
		return input == KindBool
	case UOBinNot:
		return input == KindInt || input == KindUint
	case UORef:
		return true
	case UODeref:
		return input == KindPointer
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

func NewUnaryOpNode(expression Expression, op UnaryOp) (Expression, error) {
	if !op.InputAllowed(expression.ReturnType().Kind()) {
		return UnaryOpNode{}, fmt.Errorf("Invalid opperation %s on %s", op.String(), expression.ReturnType().Name())
	}

	switch op {
	case UOPlus: // does nothing anyways
		return expression, nil
	case UONegative:
		if lit, ok := expression.(IntLit); ok {
			lit.Value = -lit.Value
			return lit, nil
		} else if lit, ok := expression.(FloatLit); ok {
			lit.Value = -lit.Value
			return lit, nil
		}

	case UORef:
		if _, ok := expression.(Var); ok {
			// ok
		} else if _, ok := expression.(StructIndex); ok {
			// ok
		} else {
			return UnaryOpNode{}, errors.New("Can only take a reference to a variable or constant")
		}

	}

	return UnaryOpNode{expression, op}, nil
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
