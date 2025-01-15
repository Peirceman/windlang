package main

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

// This file needs to be split up in different files wtf is this monster

type Kind int

const (
	KindVoid Kind = 0
	KindAny  Kind = ^0

	KindInt     Kind = 0x1
	KindUint    Kind = 0x2
	KindFloat   Kind = 0x4
	KindBool    Kind = 0x8
	KindPointer Kind = 0x10
	KindArray   Kind = 0x20

	KindNumberMask Kind = KindInt | KindUint | KindFloat

	KindString Kind = KindPointer
)

type Identifier string
type Type struct {
	kind  Kind
	size  int
	name  Identifier
	inner *Type
}

var TypeVoid = Type{kind: KindVoid}

func typesMatch(a, b Type) bool {
	aPtr, bPtr := &a, &b

	for ; aPtr != nil && bPtr != nil; aPtr, bPtr = aPtr.inner, bPtr.inner {
		if aPtr.size != bPtr.size {
			return false
		}

		if aPtr.kind != bPtr.kind {
			return false
		}

		if aPtr.name != bPtr.name {
			return false
		}
	}

	return aPtr == nil && bPtr == nil
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
	name    Identifier
	typ     Type
	isConst bool
}

var _ Expression = (*Var)(nil)

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

type Cast struct {
	inner   Expression
	newType Type
}

var _ Expression = (*Cast)(nil)

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
	if !typesMatch(lhs.returnType(), rhs.returnType()) {
		return BinaryOpNode{}, errors.New("lhs and rhs types dont match: " + string(lhs.returnType().name) + " " + string(rhs.returnType().name))
	}

	if !op.InputAllowed(lhs.returnType().kind) {
		return BinaryOpNode{}, errors.New("invalid opperation " + op.String() + " on " + string(lhs.returnType().name))
	}

	return BinaryOpNode{lhs, rhs, op}, nil
}

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
	if BOCount != 28 {
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
	case BOMod:
		return input&KindInt != 0
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

func (b BinaryOp) returnType(input Type) Type {
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
		return Type{KindBool, 4, "bool", nil}
	case BOLt:
		return Type{KindBool, 4, "bool", nil}
	case BOGtEq:
		return Type{KindBool, 4, "bool", nil}
	case BOLtEq:
		return Type{KindBool, 4, "bool", nil}
	case BOEquals:
		return Type{KindBool, 4, "bool", nil}
	case BONotEqual:
		return Type{KindBool, 4, "bool", nil}
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
	case BoAndAssign:
		return TypeVoid
	case BoOrAssign:
		return TypeVoid
	case BoXorAssign:
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

// ToBinOp converts the token type to it's
// binary opperation, returns -1 if it isn't a binary op
func (t TokenType) ToBinOp() BinaryOp {
	if BOCount != 28 {
		panic("Binary opperation enum length changed")
	}

	if TTCount != 61 {
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
	case TTPercent:
		return BOMod
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
		return input&KindBool != 0
	case UOBinNot:
		return input&KindInt != 0
	case UORef:
		return true
	case UODeref:
		return input&KindPointer != 0
	}

	panic("not a unary op")
}

func (u UnaryOp) returnType(input Type) Type {
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
		return Type{KindPointer, 8, "", &input}
	case UODeref:
		return *input.inner
	}

	panic("not a unary op")
}

func (t TokenType) ToUnOp() UnaryOp {
	if UOCount != 6 {
		panic("Unary opperation enum length changed")
	}

	if TTCount != 61 {
		panic("Token type enum length changed")
	}

	switch t {
	case TTPlus:
		return UOPlus
	case TTDash:
		return UONegative
	case TTExclam:
		return UOBoolNot
	case TTTilde:
		return UOBinNot
	case TTAmp:
		return UORef
	case TTStar:
		return UODeref
	}

	return -1
}

type UnaryOpNode struct {
	Expression Expression
	Op         UnaryOp
}

var _ Expression = (*UnaryOpNode)(nil)

func NewUnaryOpNode(expression Expression, op UnaryOp) (Expression, error) {
	if !op.InputAllowed(expression.returnType().kind) {
		return UnaryOpNode{}, fmt.Errorf("Invalid opperation %s on %s", op.String(), expression.returnType().name)
	}

	switch op {
	case UOPlus: // does nothing anyways
		return expression, nil
	case UONegative:
		if lit, ok := expression.(IntLit); ok {
			lit.value = -lit.value
			return lit, nil
		} else if lit, ok := expression.(FloatLit); ok {
			if lit.value[0] == '-' {
				lit.value = lit.value[1:]
			} else {
				lit.value = "-" + lit.value
			}
			return lit, nil
		}

	case UORef:
		if _, ok := expression.(Var); ok {
			// ok
		} else {
			return UnaryOpNode{}, errors.New("Can only take a reference to a variable or constant")
		}

	}

	return UnaryOpNode{expression, op}, nil

}

func (i IntLit) string() string {
	return strconv.Itoa(i.value)
}

func (i IntLit) returnType() Type {
	return Type{KindInt, 8, "int64", nil}
}

func (f FloatLit) string() string {
	return f.value
}

func (i FloatLit) returnType() Type {
	return Type{KindFloat, 8, "float64", nil}
}

func (f StrLit) string() string {
	return f.litteral
}

func (i StrLit) returnType() Type {
	return Type{KindString, 8, "string", &Type{KindUint, 1, "uint8", nil}}
}

func (f CharLit) string() string {
	return f.litteral
}

func (i CharLit) returnType() Type {
	return Type{KindInt, 4, "int32", nil}
}

func (f BoolLit) string() string {
	if f.value {
		return "true"
	}

	return "false"
}

func (f BoolLit) returnType() Type {
	return Type{KindBool, 4, "bool", nil}
}

func (v Var) string() string {
	return string(v.name)
}

func (v Var) returnType() Type {
	return v.typ
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

func (c Cast) string() string {
	return ""
}

func (c Cast) returnType() Type {
	return c.newType
}

func (b BinaryOpNode) string() string {
	return "(" + b.Lhs.string() + " " + b.Op.String() + " " + b.Rhs.string() + ")"
}

func (b BinaryOpNode) returnType() Type {
	return b.Op.returnType(b.Lhs.returnType())
}

func (u UnaryOpNode) string() string {
	if u.Op.OnLeftSide() {
		return fmt.Sprint(u.Op.String(), "(", u.Expression.string(), ")")
	}

	return fmt.Sprint("(", u.Expression.string(), ")", u.Op.String())
}

func (u UnaryOpNode) returnType() Type {
	return u.Op.returnType(u.Expression.returnType())
}

////////////////////
// End Expression //
////////////////////

type VarScope map[Identifier]Var
type FuncScope map[Identifier]Func

type Scope struct {
	vars  VarScope
	Funcs FuncScope
}

type VarNode struct {
	Var
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

type WhileNode struct {
	Condition Expression
	Loop      CodeBlockNode
}

var _ AstNode = (*WhileNode)(nil)

func (s *Scope) Contains(iden Identifier) bool {
	if _, contains := s.vars[iden]; contains {
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

	if val, contains := s.Funcs[iden]; contains {
		return val
	}

	return nil
}

func (s *Scope) AddVar(v Var) {
	s.vars[v.name] = v
}

func (s *Scope) AddFunc(f Func) {
	s.Funcs[f.name] = f
}

func (v VarNode) String() string {
	if v.isConst {
		if v.Value == nil {
			return "const " + string(v.name) + ": " + string(v.typ.name) + ";"
		}
		return "const " + string(v.name) + ": " + string(v.typ.name) + " = " + v.Value.string() + ";"
	} else {
		if v.Value == nil {
			return "var " + string(v.name) + ": " + string(v.typ.name) + ";"
		}
		return "var " + string(v.name) + ": " + string(v.typ.name) + " = " + v.Value.string() + ";"
	}
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

func (w WhileNode) String() string {
	sb := strings.Builder{}

	sb.WriteString("while ")
	sb.WriteString(w.Condition.string())
	sb.WriteString(" ")
	sb.WriteString(w.Loop.String())

	return sb.String()
}
