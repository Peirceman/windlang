package ast

import "strconv"

type IntLit struct {
	Value int64
}

var _ Expression = (*IntLit)(nil)

type FloatLit struct {
	Value string // float as string
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
	return SimpleType{KindInt, 8, "int64"}
}

func (f FloatLit) string() string {
	return f.Value
}

func (i FloatLit) ReturnType() Type {
	return SimpleType{KindFloat, 8, "float64"}
}

func (f StrLit) string() string {
	return f.Litteral
}

func (i StrLit) ReturnType() Type {
	return PointerType{"string", SimpleType{KindUint, 1, "uint8"}}
}

func (f CharLit) string() string {
	return f.Litteral
}

func (i CharLit) ReturnType() Type {
	return SimpleType{KindInt, 4, "int32"}
}

func (f BoolLit) string() string {
	if f.Value {
		return "true"
	}

	return "false"
}

func (f BoolLit) ReturnType() Type {
	return SimpleType{KindBool, 4, "bool"}
}
