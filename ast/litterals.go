package ast

import (
	"strconv"
	"windlang/lexer"
)

type IntLit struct {
	Value int64
	Loc_  lexer.Location
	Type  Type
}

var _ Expression = (*IntLit)(nil)

type FloatLit struct {
	Value float64
	Loc_  lexer.Location
	Type  Type
}

var _ Expression = (*FloatLit)(nil)

type StrLit struct {
	Value    string
	Litteral string
	Loc_  lexer.Location
}

var _ Expression = (*StrLit)(nil)

type CharLit struct {
	Value    rune
	Litteral string
	Loc_  lexer.Location
}

var _ Expression = (*CharLit)(nil)

type BoolLit struct {
	Value bool
	Loc_  lexer.Location
}

var _ Expression = (*BoolLit)(nil)

func (i *IntLit) string() string {
	return strconv.FormatInt(i.Value, 10)
}

func (i *IntLit) ReturnType() Type {
	return i.Type
}

func (i *IntLit) Loc() lexer.Location {
	return i.Loc_
}

func (f *FloatLit) string() string {
	return strconv.FormatFloat(f.Value, 'g', -1, 64)
}

func (f *FloatLit) ReturnType() Type {
	return f.Type
}

func (f *FloatLit) Loc() lexer.Location {
	return f.Loc_
}

func (s StrLit) string() string {
	return s.Litteral
}

func (s StrLit) ReturnType() Type {
	return TypeString
}

func (s StrLit) Loc() lexer.Location {
	return  s.Loc_
}

func (c CharLit) string() string {
	return c.Litteral
}

func (c CharLit) ReturnType() Type {
	return TypeChar
}

func (c CharLit) Loc() lexer.Location {
	return  c.Loc_
}

func (b BoolLit) string() string {
	if b.Value {
		return "true"
	}

	return "false"
}

func (b BoolLit) ReturnType() Type {
	return TypeBool
}

func (b BoolLit) Loc() lexer.Location {
	return  b.Loc_
}

