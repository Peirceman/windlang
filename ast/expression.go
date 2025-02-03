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
	return e.Expr.Loc()
}

var _ AstNode = (*ExpressionNode)(nil)

type Expression interface {
	string() string
	ReturnType() Type
	Loc() lexer.Location
}

type Var struct {
	Name    Identifier
	Typ     Type
	IsConst bool
	Loc_    lexer.Location
}

var _ Expression = (*Var)(nil)

type Func struct {
	Name    Identifier
	Args    []Var
	RetType Type
	Loc_    lexer.Location
}

var _ Expression = (*Func)(nil)

type FuncCall struct {
	Fun  Func
	Args []Expression
	Loc_ lexer.Location
}

var _ Expression = (*FuncCall)(nil)

type Cast struct {
	Inner   Expression
	NewType Type
	Loc_    lexer.Location
}

var _ Expression = (*Cast)(nil)

type StructIndex struct {
	Base   Expression
	Typ    Type
	Offset int
	Loc_   lexer.Location
}

var _ Expression = (*StructIndex)(nil)

type ArrayIndex struct {
	Array Expression
	Index Expression
	Typ   Type
	Loc_  lexer.Location
}

var _ Expression = (*ArrayIndex)(nil)

type Allocation struct {
	Typ        Type
	ElemsCount Expression
	Loc_       lexer.Location
}

var _ Expression = (*Allocation)(nil)

func (v Var) string() string {
	return string(v.Name)
}

func (v Var) ReturnType() Type {
	return v.Typ
}

func (v Var) Loc() lexer.Location {
	return v.Loc_
}

func (f Func) string() string {
	return string(f.Name)
}

func (f Func) ReturnType() Type {
	return f.RetType // TODO: change this when function objects exist
}

func (f Func) Loc() lexer.Location {
	return f.Loc_
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

func (f FuncCall) Loc() lexer.Location {
	return f.Loc_
}

func (c Cast) string() string {
	return "(" + c.Inner.string() + ": " + string(c.NewType.Name()) + ")"
}

func (c Cast) ReturnType() Type {
	return c.NewType
}

func (c Cast) Loc() lexer.Location {
	return c.Loc_
}

func (s StructIndex) string() string {
	return "" // cant really do this
}

func (s StructIndex) ReturnType() Type {
	return s.Typ
}

func (s StructIndex) Loc() lexer.Location {
	return s.Loc_
}

func (a ArrayIndex) string() string {
	return a.Array.string() + "[" + a.Index.string() + "]"
}

func (a ArrayIndex) ReturnType() Type {
	return a.Typ
}

func (a ArrayIndex) Loc() lexer.Location {
	return a.Loc_
}

func (a Allocation) string() string {
	return "alloc(...)"
}

func (a Allocation) ReturnType() Type {
	return a.Typ
}

func (a Allocation) Loc() lexer.Location {
	return a.Loc_
}
