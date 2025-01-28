package ast

import (
	"strings"
	"windlang/lexer"
)

type ExpressionNode struct {
	Expr Expression
}

func (e ExpressionNode) String() string {
	return e.Expr.string() + ";"
}

func (e ExpressionNode) Loc() lexer.Location {
	return lexer.Location{} // TODO: expression should have loc
}

var _ AstNode = (*ExpressionNode)(nil)

type Expression interface {
	string() string
	ReturnType() Type
}

type Var struct {
	Name    Identifier
	Typ     Type
	IsConst bool
}

var _ Expression = (*Var)(nil)

type Func struct {
	Name    Identifier
	Args    []Var
	RetType Type
}

var _ Expression = (*Func)(nil)

type FuncCall struct {
	Fun  Func
	Args []Expression
}

var _ Expression = (*FuncCall)(nil)

type Cast struct {
	Inner   Expression
	NewType Type
}

var _ Expression = (*Cast)(nil)

type StructIndex struct {
	Base   Expression
	Typ    Type
	Offset int
}

var _ Expression = (*StructIndex)(nil)

type ArrayIndex struct {
	Array Expression
	Index Expression
	Typ   Type
}

var _ Expression = (*ArrayIndex)(nil)

type Allocation struct {
	Typ        Type
	ElemsCount Expression
}

var _ Expression = (*Allocation)(nil)

func (v Var) string() string {
	return string(v.Name)
}

func (v Var) ReturnType() Type {
	return v.Typ
}

func (f Func) string() string {
	return string(f.Name)
}

func (f Func) ReturnType() Type {
	return f.RetType // TODO: change this when function objects exist
}

func (f FuncCall) string() string {
	sb := strings.Builder{}
	sb.WriteString(string(f.Fun.Name))
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

func (f FuncCall) ReturnType() Type {
	return f.Fun.ReturnType()
}

func (c Cast) string() string {
	return "(" + c.Inner.string() + ": " + string(c.NewType.Name()) + ")"
}

func (c Cast) ReturnType() Type {
	return c.NewType
}

func (s StructIndex) string() string {
	return "" // cant really do this
}

func (s StructIndex) ReturnType() Type {
	return s.Typ
}

func (a ArrayIndex) string() string {
	return a.Array.string() + "[" + a.Index.string() + "]"
}

func (a ArrayIndex) ReturnType() Type {
	return a.Typ
}

func (a Allocation) string() string {
	return "alloc(...)"
}

func (a Allocation) ReturnType() Type {
	return a.Typ
}
