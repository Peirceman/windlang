package ast

import "strconv"

type IntLit struct {
	Value int64
}

var _ Expression = (*IntLit)(nil)

type FloatLit struct {
	Value float64
}

var _ Expression = (*FloatLit)(nil)

type StrLit struct {
	Value    string
	Litteral string
}

var _ Expression = (*StrLit)(nil)

type CharLit struct {
	Value    rune
	Litteral string
}

var _ Expression = (*CharLit)(nil)

type BoolLit struct {
	Value bool
}

var _ Expression = (*BoolLit)(nil)


func (i IntLit) string() string {
	return strconv.FormatInt(i.Value, 10)
}

func (i IntLit) ReturnType() Type {
	return TypeInt64
}

func (f FloatLit) string() string {
	return strconv.FormatFloat(f.Value, 'g', -1, 64)
}

func (i FloatLit) ReturnType() Type {
	return TypeFloat64
}

func (f StrLit) string() string {
	return f.Litteral
}

func (i StrLit) ReturnType() Type {
	return TypeString
}

func (f CharLit) string() string {
	return f.Litteral
}

func (i CharLit) ReturnType() Type {
	return TypeChar
}

func (f BoolLit) string() string {
	if f.Value {
		return "true"
	}

	return "false"
}

func (f BoolLit) ReturnType() Type {
	return TypeBool
}
