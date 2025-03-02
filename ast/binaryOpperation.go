package ast

import "windlang/lexer"

type BinaryOp int

const (
	BOPlus BinaryOp = iota
	BOMinus
	BOMul
	BODiv
	BOMod

	BOBinAnd
	BOBinOr
	BOBinXor

	BOBoolAnd
	BOBoolOr

	BOShl
	BOShr

	BOGt
	BOLt
	BOGtEq
	BOLtEq

	BOEquals
	BONotEqual

	BOAssign
	BOPlusAssign
	BODashAssign
	BOStarAssign
	BOSlashAssign
	BOAndAssign
	BOOrAssign
	BOXorAssign
	BOShrAssign
	BOShlAssign

	BOCount
)

type BinaryOpNode struct {
	Lhs, Rhs Expression
	Op       BinaryOp
	Loc_ lexer.Location
}

var _ Expression = (*BinaryOpNode)(nil)

func (b BinaryOp) String() string {
	if BOCount != 28 {
		panic("Binary Opperation enum length changed")
	}

	switch b {
	case BOPlus:
		return "+"
	case BOMinus:
		return "-"
	case BOMul:
		return "*"
	case BODiv:
		return "/"
	case BOMod:
		return "%"
	case BOBinAnd:
		return "&"
	case BOBinOr:
		return "|"
	case BOBinXor:
		return "^"
	case BOBoolAnd:
		return "&&"
	case BOBoolOr:
		return "||"
	case BOShl:
		return "<<"
	case BOShr:
		return ">>"
	case BOGt:
		return ">"
	case BOLt:
		return "<"
	case BOGtEq:
		return ">="
	case BOLtEq:
		return "<="
	case BOEquals:
		return "=="
	case BONotEqual:
		return "!="
	case BOAssign:
		return "="
	case BOPlusAssign:
		return "+="
	case BODashAssign:
		return "-="
	case BOStarAssign:
		return "*="
	case BOSlashAssign:
		return "/="
	case BOAndAssign:
		return "&="
	case BOOrAssign:
		return "|="
	case BOXorAssign:
		return "^="
	case BOShrAssign:
		return ">>="
	case BOShlAssign:
		return "<<="
	}
	panic("Not a binary op")
}

const MaxPrecedence = 10

/* Precedence gives the precednece of the opperator
 * in general, precedence goes:
 * first: boolean instructions
 * then: bitwise instructions
 * third: comparison
 * fourth: shift
 * lastly: arithmatic instructions
 */
func (b BinaryOp) Precedence() int {
	if BOCount != 28 {
		panic("Binary Opperation enum length changed")
	}

	switch b {
	case BOPlus:
		return 9
	case BOMinus:
		return 9
	case BOMul:
		return 10
	case BODiv:
		return 10
	case BOMod:
		return 10
	case BOBinAnd:
		return 5
	case BOBinOr:
		return 3
	case BOBinXor:
		return 4
	case BOBoolAnd:
		return 2
	case BOBoolOr:
		return 1
	case BOShl:
		return 8
	case BOShr:
		return 8
	case BOGt:
		return 7
	case BOLt:
		return 7
	case BOGtEq:
		return 7
	case BOLtEq:
		return 7
	case BOEquals:
		return 6
	case BONotEqual:
		return 6
	case BOAssign:
		return 0
	case BOPlusAssign:
		return 0
	case BODashAssign:
		return 0
	case BOStarAssign:
		return 0
	case BOSlashAssign:
		return 0
	case BOAndAssign:
		return 0
	case BOOrAssign:
		return 0
	case BOXorAssign:
		return 0
	case BOShrAssign:
		return 0
	case BOShlAssign:
		return 0
	}

	panic("illegal")
}

func (b BinaryOp) ReturnType(input Type) Type {
	if BOCount != 28 {
		panic("Binary Opperation enum length changed")
	}

	switch b {
	case BOPlus:
		return input
	case BOMinus:
		return input
	case BOMul:
		return input
	case BODiv:
		return input
	case BOMod:
		return input
	case BOBinAnd:
		return input
	case BOBinOr:
		return input
	case BOBinXor:
		return input
	case BOBoolAnd:
		return input
	case BOBoolOr:
		return input
	case BOShl:
		return input
	case BOShr:
		return input
	case BOGt:
		return TypeBool
	case BOLt:
		return TypeBool
	case BOGtEq:
		return TypeBool
	case BOLtEq:
		return TypeBool
	case BOEquals:
		return TypeBool
	case BONotEqual:
		return TypeBool
	case BOAssign:
		return TypeVoid
	case BOPlusAssign:
		return TypeVoid
	case BODashAssign:
		return TypeVoid
	case BOStarAssign:
		return TypeVoid
	case BOSlashAssign:
		return TypeVoid
	case BOAndAssign:
		return TypeVoid
	case BOOrAssign:
		return TypeVoid
	case BOXorAssign:
		return TypeVoid
	case BOShrAssign:
		return TypeVoid
	case BOShlAssign:
		return TypeVoid
	}

	panic("illegal")
}

func LeftToRight(precedence int) bool {
	switch precedence {
	case 0:
		return false
	case 1:
		return true
	case 2:
		return true
	case 3:
		return true
	case 4:
		return true
	case 5:
		return true
	case 6:
		return true
	case 7:
		return true
	case 8:
		return true
	case 9:
		return true
	case 10:
		return true
	}

	panic("illegal")
}

func (b BinaryOpNode) string() string {
	return "(" + b.Lhs.string() + " " + b.Op.String() + " " + b.Rhs.string() + ")"
}

func (b BinaryOpNode) ReturnType() Type {
	return b.Op.ReturnType(b.Lhs.ReturnType())
}

func (b BinaryOpNode) Loc() lexer.Location {
	return b.Loc_
}

