package ast

import (
	"strings"
)

type AstNode interface {
	String() string
}

type VarNode struct {
	Var
	Value Expression
}

var _ AstNode = (*VarNode)(nil)

type FuncNode struct {
	Name       Identifier
	Args       []Var
	ReturnType Type
	Body       CodeBlockNode
}

var _ AstNode = (*FuncNode)(nil)

type ReturnNode struct {
	Expr Expression // optional
}

var _ AstNode = (*ReturnNode)(nil)

type VarScope map[Identifier]Var
type FuncScope map[Identifier]Func

type Scope struct {
	Vars  VarScope
	Funcs FuncScope
}

type CodeBlockNode struct {
	Statements []AstNode
	Scope      Scope
}

var _ AstNode = (*CodeBlockNode)(nil)

type IfChain struct {
	IfCondition Expression
	IfStatement CodeBlockNode

	ElifConditions []Expression
	ElifStatements []CodeBlockNode

	HasElse       bool
	ElseStatement CodeBlockNode
}

var _ AstNode = (*IfChain)(nil)

type WhileNode struct {
	Condition Expression
	Body      CodeBlockNode
}

var _ AstNode = (*WhileNode)(nil)

func (s *Scope) Contains(iden Identifier) bool {
	if _, contains := s.Vars[iden]; contains {
		return true
	}

	if _, contains := s.Funcs[iden]; contains {
		return true
	}

	return false
}

// Var or Func
func (s *Scope) Get(iden Identifier) Expression {
	if val, contains := s.Vars[iden]; contains {
		return val
	}

	if val, contains := s.Funcs[iden]; contains {
		return val
	}

	return nil
}

func (s *Scope) AddVar(v Var) {
	s.Vars[v.Name] = v
}

func (s *Scope) AddFunc(f Func) {
	s.Funcs[f.Name] = f
}

func (v VarNode) String() string {
	if v.IsConst {
		if v.Value == nil {
			return "const " + string(v.Name) + ": " + string(v.Typ.Name()) + ";"
		}
		return "const " + string(v.Name) + ": " + string(v.Typ.Name()) + " = " + v.Value.string() + ";"
	} else {
		if v.Value == nil {
			return "var " + string(v.Name) + ": " + string(v.Typ.Name()) + ";"
		}
		return "var " + string(v.Name) + ": " + string(v.Typ.Name()) + " = " + v.Value.string() + ";"
	}
}

func (f FuncNode) String() string {
	sb := strings.Builder{}

	sb.WriteString("fn ")
	sb.WriteString(string(f.Name))
	sb.WriteRune('(')

	if len(f.Args) > 0 {
		sb.WriteString(string(f.Args[0].Name))
		sb.WriteString(": ")
		sb.WriteString(string(f.Args[0].Typ.Name()))
		for _, arg := range f.Args[1:] {
			sb.WriteString(", ")
			sb.WriteString(string(arg.Name))
			sb.WriteString(": ")
			sb.WriteString(string(arg.Typ.Name()))
		}
	}

	sb.WriteRune(')')

	if f.ReturnType.Kind() != KindVoid {
		sb.WriteString(": ")
		sb.WriteString(string(f.ReturnType.Name()))
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

	if c.HasElse {
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
	sb.WriteString(w.Body.String())

	return sb.String()
}
