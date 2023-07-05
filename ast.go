package main

import (
	"strconv"
	"strings"
)

type Identifier string
type Type Identifier

type AstNode interface {
	String() string
}

// //////////////
// Expression //
// //////////////
type Expression interface {
	String() string
}

var _ AstNode = (Expression)(nil)

type IntLit struct {
	value int
}

var _ Expression = (*IntLit)(nil)

type FloatLit struct {
	value string // float as string
}

var _ Expression = (*FloatLit)(nil)

type VarLit struct {
	varname string
}

var _ Expression = (*VarLit)(nil)

type Assignment struct {
	name  Identifier
	value Expression
}

type BinaryOp int

const (
	BOPlus BinaryOp = iota
	BOMinus
	BOMul
	BODiv

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
	BoAndAssign
	BoOrAssign
	BoXorAssign
	BOShrAssign
	BOShlAssign

	BOCount
)

type BinaryOpNode struct {
	lhs, rhs Expression
	op       BinaryOp
}

var _ Expression = (*BinaryOpNode)(nil)

func (b BinaryOp) String() string {
	if BOCount != 27 {
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
	case BoAndAssign:
		return "&="
	case BoOrAssign:
		return "|="
	case BoXorAssign:
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
	if BOCount != 27 {
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
	case BoAndAssign:
		return 0
	case BoOrAssign:
		return 0
	case BoXorAssign:
		return 0
	case BOShrAssign:
		return 0
	case BOShlAssign:
		return 0
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

// TokenTypeToBinOp converts the token type to it's
// binary opperation, returns -1 if it isn't a binary op
func (t TokenType) TokenTypeToBinOp() BinaryOp {
	if BOCount != 27 {
		panic("Binary opperation enum length changed")
	}

	if TTCount != 58 {
		panic("Token type enum length changed")
	}

	switch t {
	case TTAssign:
		return BOAssign
	case TTPlus:
		return BOPlus
	case TTDash:
		return BOMinus
	case TTStar:
		return BOMul
	case TTSlash:
		return BODiv
	case TTPlusAssign:
		return BOPlusAssign
	case TTDashAssign:
		return BODashAssign
	case TTStarAssign:
		return BOStarAssign
	case TTSlashAssign:
		return BOSlashAssign
	case TTAmp:
		return BOBinAnd
	case TTBar:
		return BOBinOr
	case TTCaret:
		return BOBinXor
	case TTAnd:
		return BOBoolAnd
	case TTOr:
		return BOBoolOr
	case TTGt:
		return BOGt
	case TTLt:
		return BOLt
	case TTGtEq:
		return BOGtEq
	case TTLtEq:
		return BOLtEq
	case TTShr:
		return BOShr
	case TTShl:
		return BOShl
	case TTAmpAssign:
		return BoAndAssign
	case TTBarAssign:
		return BoOrAssign
	case TTCaretAssign:
		return BoXorAssign
	case TTShrAssign:
		return BOShrAssign
	case TTShlAssign:
		return BOShlAssign
	case TTEqual:
		return BOEquals
	case TTNotEqual:
		return BONotEqual
	}

	return -1
}

var _ Expression = (*Assignment)(nil)

func (i IntLit) String() string {
	return strconv.Itoa(i.value)
}

func (f FloatLit) String() string {
	return f.value
}

func (v VarLit) String() string {
	return v.varname
}

func (c Assignment) String() string {
	return string(c.name) + " = " + c.value.String() + ";"
}

func (b BinaryOpNode) String() string {
	return "(" + b.lhs.String() + " " + b.op.String() + " " + b.rhs.String() + ")"
}

////////////////////
// End Expression //
////////////////////

type ConstNode struct {
	name  Identifier
	typ   Type
	value Expression
}

var _ AstNode = (*ConstNode)(nil)

type VarNode struct {
	name  Identifier
	typ   Type
	value Expression
}

var _ AstNode = (*VarNode)(nil)

type CodeBlockNode struct {
	Statements []AstNode
}

type IfChain struct {
	ifCondition    Expression
	ifStatement    CodeBlockNode
	elifConditions []Expression
	elifStatement  []CodeBlockNode
	elseCondition  Expression // nil if no else condition
	elseStatement  CodeBlockNode
}

var _ AstNode = (*IfChain)(nil)

func (c ConstNode) String() string {
	return "const " + string(c.name) + ": " + string(c.typ) + " = " + c.value.String() + ";"
}

func (c VarNode) String() string {
	return "var " + string(c.name) + ": " + string(c.typ) + " = " + c.value.String() + ";"
}

func (c IfChain) String() string {
	sb := strings.Builder{}
	sb.WriteString("if ")
	sb.WriteString(c.ifCondition.String())
	sb.WriteString("{\n\t")

	return sb.String()
}
