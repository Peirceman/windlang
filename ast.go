package main

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

/*
 * Kind is an integer which stores all the built-in types and some extra info
 * Note: multiple pieces of information are stored in the Kind, so it should
 * never be compared with `==` but with a mask and != 0
 * for example:
 * WRONG: k == KindInt
 * RIGHT: k & KindInt != 0
 */
type Kind int

const (
	KindVoid Kind = 0
	KindAny  Kind = ^0

	Kind8        Kind = 0x1
	Kind16       Kind = 0x2
	Kind32       Kind = 0x4
	Kind64       Kind = 0x8
	KindSizeMask Kind = 0xf

	// TODO: update string Kind when pointer and array types are implemented
	// TODO: add struct Kind
	// TODO: no unsigned int
	KindInt        Kind = 0x10
	KindFloat      Kind = 0x20
	KindBool       Kind = 0x40 | Kind32
	KindString     Kind = 0x80 | Kind64
	KindTypeMask   Kind = 0xf0
	KindNumberMask Kind = KindInt | KindFloat

	KindInt8    Kind = KindInt | Kind8
	KindInt16   Kind = KindInt | Kind16
	KindInt32   Kind = KindInt | Kind32
	KindInt64   Kind = KindInt | Kind64
	KindFloat32 Kind = KindFloat | Kind32
	KindFloat64 Kind = KindFloat | Kind64

	// reserved, but not used
	KindPointer Kind = 0x100
)

func KindFromString(str string) Kind {
	switch str {
	case "int8":
		return KindInt8
	case "int16":
		return KindInt16
	case "int32":
		return KindInt32
	case "int64", "int": // TODO: `int` and `float` shouldn't just return the 64 bit variation
		return KindInt64
	case "float32":
		return KindFloat32
	case "float64", "float":
		return KindFloat64
	case "bool":
		return KindBool
	case "string":
		return KindString
	case "any":
		return KindAny
	default:
		return KindVoid
	}

}

func (k Kind) String() string {
	switch k {
	case KindBool:
		return "bool"
	case KindString:
		return "string"
	case KindInt8:
		return "int8"
	case KindInt16:
		return "int16"
	case KindInt32:
		return "int32"
	case KindInt64:
		return "int64"
	case KindFloat32:
		return "float32"
	case KindFloat64:
		return "float64"
	case KindAny:
		return "any"
	default:
		panic(fmt.Sprintf("Unkown kind: 0x%x", int(k)))
	}
}

type Identifier string
type Type struct {
	kind Kind
	name Identifier // unused until user-defined types exist
}

type AstNode interface {
	String() string
}

/////////////////
//  Expression //
/////////////////

type ExpressionNode struct {
	Expr Expression
}

func (e ExpressionNode) String() string {
	return e.Expr.string() + ";"
}

var _ AstNode = (*ExpressionNode)(nil)

type Expression interface {
	string() string
	returnType() Type
}

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

type Var struct {
	name Identifier
	typ  Type
}

var _ Expression = (*Var)(nil)

type Const struct {
	name Identifier
	typ  Type
}

var _ Expression = (*Const)(nil)

type Func struct {
	name    Identifier
	Args    []Var
	retType Type
}

var _ Expression = (*Func)(nil)

type FuncCall struct {
	fun  Func
	Args []Expression
}

var _ Expression = (*FuncCall)(nil)

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
	Lhs, Rhs Expression
	Op       BinaryOp
}

var _ Expression = (*BinaryOpNode)(nil)

func NewBinaryOpNode(lhs, rhs Expression, op BinaryOp) (BinaryOpNode, error) {
	if lhs.returnType().kind != rhs.returnType().kind {
		return BinaryOpNode{}, errors.New("lhs and rhs types dont match: " + lhs.returnType().kind.String() + " " + rhs.returnType().kind.String())
	}

	if !op.InputAllowed(lhs.returnType().kind) {
		return BinaryOpNode{}, errors.New("invalid opperation " + op.String() + " on " + lhs.returnType().kind.String())
	}

	return BinaryOpNode{lhs, rhs, op}, nil
}

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

func (b BinaryOp) InputAllowed(input Kind) bool {
	if BOCount != 27 {
		panic("Binary Opperation enum length changed")
	}

	switch b {
	case BOPlus:
		return input&KindNumberMask != 0
	case BOMinus:
		return input&KindNumberMask != 0
	case BOMul:
		return input&KindNumberMask != 0
	case BODiv:
		return input&KindNumberMask != 0
	case BOBinAnd:
		return input&KindInt != 0
	case BOBinOr:
		return input&KindInt != 0
	case BOBinXor:
		return input&KindInt != 0
	case BOBoolAnd:
		return input&KindBool != 0
	case BOBoolOr:
		return input&KindBool != 0
	case BOShl:
		return input&KindInt != 0
	case BOShr:
		return input&KindInt != 0
	case BOGt:
		return input&KindInt != 0
	case BOLt:
		return input&KindInt != 0
	case BOGtEq:
		return input&KindInt != 0
	case BOLtEq:
		return input&KindInt != 0
	case BOEquals:
		return true
	case BONotEqual:
		return true
	case BOAssign:
		return true
	case BOPlusAssign:
		return input&KindNumberMask != 0
	case BODashAssign:
		return input&KindNumberMask != 0
	case BOStarAssign:
		return input&KindNumberMask != 0
	case BOSlashAssign:
		return input&KindNumberMask != 0
	case BoAndAssign:
		return input&KindInt != 0
	case BoOrAssign:
		return input&KindInt != 0
	case BoXorAssign:
		return input&KindInt != 0
	case BOShrAssign:
		return input&KindInt != 0
	case BOShlAssign:
		return input&KindInt != 0
	}

	panic("illegal")
}
func (b BinaryOp) returnType(input Kind) Kind {
	if BOCount != 27 {
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
		return KindBool
	case BOLt:
		return KindBool
	case BOGtEq:
		return KindBool
	case BOLtEq:
		return KindBool
	case BOEquals:
		return KindBool
	case BONotEqual:
		return KindBool
	case BOAssign:
		return input
	case BOPlusAssign:
		return input
	case BODashAssign:
		return input
	case BOStarAssign:
		return input
	case BOSlashAssign:
		return input
	case BoAndAssign:
		return input
	case BoOrAssign:
		return input
	case BoXorAssign:
		return input
	case BOShrAssign:
		return input
	case BOShlAssign:
		return input
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

func (i IntLit) returnType() Type {
	kind := KindInt | Kind64
	return Type{kind, Identifier(kind.String())}
}

func (f FloatLit) string() string {
	return f.value
}

func (i FloatLit) returnType() Type {
	kind := KindFloat | Kind64
	return Type{kind, Identifier(kind.String())}
}

func (f StrLit) string() string {
	return f.litteral
}

func (i StrLit) returnType() Type {
	kind := KindString
	return Type{kind, Identifier(kind.String())}
}

func (f CharLit) string() string {
	return f.litteral
}

func (i CharLit) returnType() Type {
	kind := KindInt | Kind32
	return Type{kind, Identifier(kind.String())}
}

func (f BoolLit) string() string {
	if f.value {
		return "true"
	}

	return "false"
}

func (f BoolLit) returnType() Type {
	kind := KindBool
	return Type{kind, Identifier(kind.String())}
}

func (v Var) string() string {
	return string(v.name)
}

func (v Var) returnType() Type {
	return v.typ
}

func (c Const) string() string {
	return string(c.name)
}

func (c Const) returnType() Type {
	return c.typ
}
func (f Func) string() string {
	return string(f.name)
}

func (f Func) returnType() Type {
	return f.retType // TODO: change this when function objects exist
}

func (f FuncCall) string() string {
	sb := strings.Builder{}
	sb.WriteString(string(f.fun.name))
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

func (f FuncCall) returnType() Type {
	return f.fun.returnType()
}

func (b BinaryOpNode) string() string {
	return "(" + b.Lhs.string() + " " + b.Op.String() + " " + b.Rhs.string() + ")"
}

func (b BinaryOpNode) returnType() Type {
	kind := b.Op.returnType(b.Lhs.returnType().kind)
	return Type{kind, Identifier(kind.String())}
}

////////////////////
// End Expression //
////////////////////

type ConstScope map[Identifier]Const
type VarScope map[Identifier]Var
type FuncScope map[Identifier]Func

type Scope struct {
	vars   VarScope
	consts ConstScope
	Funcs  FuncScope
}

type ConstNode struct {
	name  Identifier
	typ   Type
	Value Expression
}

var _ AstNode = (*ConstNode)(nil)

type VarNode struct {
	name  Identifier
	typ   Type
	Value Expression
}

var _ AstNode = (*VarNode)(nil)

type FuncNode struct {
	name       Identifier
	Args       []Var
	returnType Type
	Body       CodeBlockNode
}

var _ AstNode = (*FuncNode)(nil)

type ReturnNode struct {
	Expr Expression // optional
}

var _ AstNode = (*ReturnNode)(nil)

type CodeBlockNode struct {
	Statements []AstNode
	scope      Scope
}

var _ AstNode = (*CodeBlockNode)(nil)

type IfChain struct {
	IfCondition Expression
	IfStatement CodeBlockNode

	ElifConditions []Expression
	ElifStatements []CodeBlockNode

	hasElse       bool
	ElseStatement CodeBlockNode
}

var _ AstNode = (*IfChain)(nil)

func (s *Scope) Contains(iden Identifier) bool {
	if _, contains := s.vars[iden]; contains {
		return true
	}

	if _, contains := s.consts[iden]; contains {
		return true
	}

	if _, contains := s.Funcs[iden]; contains {
		return true
	}

	return false
}

func (s *Scope) Get(iden Identifier) any {
	if val, contains := s.vars[iden]; contains {
		return val
	}

	if val, contains := s.consts[iden]; contains {
		return val
	}

	if val, contains := s.Funcs[iden]; contains {
		return val
	}

	return nil
}

func (s *Scope) AddVar(v Var) {
	s.vars[v.name] = v
}

func (s *Scope) AddConst(c Const) {
	s.consts[c.name] = c
}

func (s *Scope) AddFunc(f Func) {
	s.Funcs[f.name] = f
}

func (c ConstNode) String() string {
	if c.Value == nil {
		return "const " + string(c.name) + ": " + string(c.typ.name) + ";"
	}
	return "const " + string(c.name) + ": " + string(c.typ.name) + " = " + c.Value.string() + ";"
}

func (v VarNode) String() string {
	if v.Value == nil {
		return "var " + string(v.name) + ": " + string(v.typ.name) + ";"
	}
	return "var " + string(v.name) + ": " + string(v.typ.name) + " = " + v.Value.string() + ";"
}

func (f FuncNode) String() string {
	sb := strings.Builder{}

	sb.WriteString("fn ")
	sb.WriteString(string(f.name))
	sb.WriteRune('(')

	if len(f.Args) > 0 {
		sb.WriteString(string(f.Args[0].name))
		sb.WriteString(": ")
		sb.WriteString(string(f.Args[0].typ.name))
		for _, arg := range f.Args[1:] {
			sb.WriteString(", ")
			sb.WriteString(string(arg.name))
			sb.WriteString(": ")
			sb.WriteString(string(arg.typ.name))
		}
	}

	sb.WriteRune(')')

	if f.returnType.kind != KindVoid {
		sb.WriteString(": ")
		sb.WriteString(string(f.returnType.name))
	}

	sb.WriteRune(' ')
	sb.WriteString(f.Body.String())

	return sb.String()
}

func (r ReturnNode) String() string {
	return "return " + r.Expr.string() + ";"
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
	sb.WriteString(c.IfCondition.string())
	sb.WriteString(" ")
	sb.WriteString(c.IfStatement.String())

	for i := range c.ElifConditions {
		sb.WriteString(" elif ")
		sb.WriteString(c.ElifConditions[i].string())
		sb.WriteString(" ")
		sb.WriteString(c.ElifStatements[i].String())
	}

	if c.hasElse {
		sb.WriteString(" else ")
		sb.WriteString(c.ElseStatement.String())
	}

	return sb.String()
}
