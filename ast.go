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

/////////////////
//  Expression //
/////////////////

type ExpressionNode struct {
	expr Expression
}

func (e ExpressionNode) String() string {
	return e.expr.string() + ";"
}

type Expression interface {
	string() string
}

var _ AstNode = (*ExpressionNode)(nil)

type IntLit struct {
	value int
}

var _ Expression = (*IntLit)(nil)

type FloatLit struct {
	value string // float as string
}

var _ Expression = (*FloatLit)(nil)

type StrLit struct {
	value    string
	litteral string
}

var _ Expression = (*StrLit)(nil)

type CharLit struct {
	value    rune
	litteral string
}

var _ Expression = (*CharLit)(nil)

type BoolLit struct {
	value bool
}

var _ Expression = (*BoolLit)(nil)

type VarLit struct {
	varname Identifier
}

var _ Expression = (*VarLit)(nil)

type FuncCall struct {
	funcName Identifier
	Args     []Expression
}

var _ Expression = (*FuncCall)(nil)

// TODO: use Assignment struct instead of binary op for assignment
type Assignment struct {
	name  Identifier
	value Expression
}

var _ Expression = (*Assignment)(nil)

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

func (b BinaryOp) string() string {
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

	if TTCount != 59 {
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

func (i IntLit) string() string {
	return strconv.Itoa(i.value)
}

func (f FloatLit) string() string {
	return f.value
}

func (f StrLit) string() string {
	return f.litteral
}

func (f CharLit) string() string {
	return f.litteral
}

func (f BoolLit) string() string {
	if f.value {
		return "true"
	}

	return "false"
}

func (v VarLit) string() string {
	return string(v.varname)
}

func (f FuncCall) string() string {
	sb := strings.Builder{}
	sb.WriteString(string(f.funcName))
	sb.WriteRune('(')

	if len(f.Args) > 0 {
		sb.WriteString(f.Args[0].string())

		for _, arg := range f.Args[1:] {
			sb.WriteString(", ")
			sb.WriteString(arg.string())
		}

	}

	sb.WriteRune(')')

	return sb.String()
}

func (c Assignment) string() string {
	return string(c.name) + " = " + c.value.string()
}

func (b BinaryOpNode) string() string {
	return "(" + b.lhs.string() + " " + b.op.string() + " " + b.rhs.string() + ")"
}

////////////////////
// End Expression //
////////////////////

type Var struct {
	name Identifier
	typ  Type
}

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

type FnNode struct {
	name       Identifier
	Args       []Var
	returnType Type
	body       CodeBlockNode
}

var _ AstNode = (*FnNode)(nil)

type ReturnNode struct {
	expr Expression
}

var _ AstNode = (*ReturnNode)(nil)

type CodeBlockNode struct {
	Statements []AstNode
}

var _ AstNode = (*CodeBlockNode)(nil)

type IfChain struct {
	ifCondition Expression
	ifStatement CodeBlockNode

	elifConditions []Expression
	elifStatements []CodeBlockNode

	hasElse       bool
	elseStatement CodeBlockNode
}

var _ AstNode = (*IfChain)(nil)

func (c ConstNode) String() string {
	return "const " + string(c.name) + ": " + string(c.typ) + " = " + c.value.string() + ";"
}

func (v VarNode) String() string {
	return "var " + string(v.name) + ": " + string(v.typ) + " = " + v.value.string() + ";"
}

func (f FnNode) String() string {
	sb := strings.Builder{}

	sb.WriteString("fn ")
	sb.WriteString(string(f.name))
	sb.WriteRune('(')

	if len(f.Args) > 0 {
		sb.WriteString(string(f.Args[0].name))
		sb.WriteString(": ")
		sb.WriteString(string(f.Args[0].typ))
		for _, arg := range f.Args[1:] {
			sb.WriteString(", ")
			sb.WriteString(string(arg.name))
			sb.WriteString(": ")
			sb.WriteString(string(arg.typ))
		}
	}

	sb.WriteRune(')')

	if f.returnType != "" {
		sb.WriteString(": ")
		sb.WriteString(string(f.returnType))
	}

	sb.WriteRune(' ')
	sb.WriteString(f.body.String())

	return sb.String()
}

func (r ReturnNode) String() string {
	return "return " + r.expr.string() + ";"
}

func (b CodeBlockNode) String() string {
	sb := strings.Builder{}
	sb.WriteRune('{')
	for _, node := range b.Statements {
		sb.WriteString("\n\t")
		sb.WriteString(strings.ReplaceAll(node.String(), "\n", "\n\t"))
	}
	sb.WriteString("\n}")

	return sb.String()
}

func (c IfChain) String() string {
	sb := strings.Builder{}

	sb.WriteString("if ")
	sb.WriteString(c.ifCondition.string())
	sb.WriteString(" ")
	sb.WriteString(c.ifStatement.String())

	for i := range c.elifConditions {
		sb.WriteString(" elif ")
		sb.WriteString(c.elifConditions[i].string())
		sb.WriteString(" ")
		sb.WriteString(c.elifStatements[i].String())
	}

	if c.hasElse {
		sb.WriteString(" else ")
		sb.WriteString(c.elseStatement.String())
	}

	return sb.String()
}
