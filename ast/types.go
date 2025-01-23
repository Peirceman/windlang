package ast

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
	KindStruct  Kind = 0x40

	KindNumberMask Kind = KindInt | KindUint | KindFloat

	KindString Kind = KindPointer
)

type Identifier string
type Type interface {
	Kind() Kind
	Size() int
	Name() Identifier
	SetName(Identifier) Type
	matches(other Type) bool
}

type SimpleType struct {
	Kind_ Kind
	Size_ int
	Name_ Identifier
}

var _ Type = (*SimpleType)(nil)

type PointerType struct {
	Name_ Identifier
	Inner Type
}

var _ Type = (*PointerType)(nil)

type StructField struct {
	Var
	Offset int
}

type StructType struct {
	Name_  Identifier
	Size_  int
	Fields []StructField // TODO: padding
}

var _ Type = (*StructType)(nil)

type ArrayType struct {
	Name_ Identifier
	Inner Type
}

var _ Type = (*ArrayType)(nil)

// Default types
var (
	TypeVoid = SimpleType{KindVoid, 0, ""}

	TypeInt8  = SimpleType{KindInt, 1, "int8"}
	TypeInt16 = SimpleType{KindInt, 2, "int16"}
	TypeInt32 = SimpleType{KindInt, 4, "int32"}
	TypeInt64 = SimpleType{KindInt, 8, "int64"}

	TypeUint8  = SimpleType{KindUint, 1, "uint8"}
	TypeUint16 = SimpleType{KindUint, 2, "uint16"}
	TypeUint32 = SimpleType{KindUint, 4, "uint32"}
	TypeUint64 = SimpleType{KindUint, 8, "uint64"}

	TypeFloat32 = SimpleType{KindFloat, 4, "float32"}
	TypeFloat64 = SimpleType{KindFloat, 8, "float64"}

	TypeBool   = SimpleType{KindBool, 4, "bool"}
	TypeChar   = SimpleType{KindInt, 4, "char"}
	TypeString = PointerType{"string", TypeUint8}
)

func EqualTypes(a, b Type) bool {
	switch a.(type) {
	case SimpleType:
		_, ok := b.(SimpleType)
		return ok && a.matches(b)
	case PointerType:
		_, ok := b.(PointerType)
		return ok && a.matches(b)
	case StructType:
		_, ok := b.(StructType)
		return ok && a.matches(b)
	case ArrayType:
		_, ok := b.(ArrayType)
		return ok && a.matches(b)
	}

	panic("unknow Type")
}

func (s SimpleType) Kind() Kind              { return s.Kind_ }
func (s SimpleType) Size() int               { return s.Size_ }
func (s SimpleType) Name() Identifier        { return s.Name_ }
func (s SimpleType) matches(other Type) bool { return s == other.(SimpleType) }

func (s SimpleType) SetName(iden Identifier) Type {
	s.Name_ = iden
	return s
}

func (p PointerType) Kind() Kind       { return KindPointer }
func (p PointerType) Size() int        { return 8 }
func (p PointerType) Name() Identifier { return p.Name_ }

func (p PointerType) SetName(iden Identifier) Type {
	p.Name_ = iden
	return p
}

func (p PointerType) matches(other Type) bool {
	return EqualTypes(p.Inner, other.(PointerType).Inner)
}

func (s StructType) Kind() Kind       { return KindStruct }
func (s StructType) Size() int        { return s.Size_ }
func (s StructType) Name() Identifier { return s.Name_ }

func (s StructType) SetName(iden Identifier) Type {
	s.Name_ = iden
	return s
}

func (s StructType) matches(other Type) bool {
	otherS := other.(StructType)
	if s.Name_ != otherS.Name_ {
		return false
	}

	for i, fieldA := range s.Fields {
		fieldB := otherS.Fields[i]
		if fieldA.Name != fieldB.Name ||
			fieldA.Offset != fieldB.Offset ||
			!EqualTypes(fieldA.Typ, fieldB.Typ) {

			return false
		}
	}

	return true
}

// optional
func (s StructType) GetField(iden Identifier) *StructField {
	for _, field := range s.Fields {
		if field.Name == iden {
			return &field
		}
	}

	return nil
}

func (s ArrayType) Kind() Kind       { return KindArray }
func (s ArrayType) Size() int        { return 24 }
func (s ArrayType) Name() Identifier { return s.Name_ }

func (s ArrayType) SetName(iden Identifier) Type {
	s.Name_ = iden
	return s
}

func (s ArrayType) matches(other Type) bool {
	otherS := other.(ArrayType)

	return s.Name_ == otherS.Name_ && EqualTypes(s.Inner, otherS.Inner)
}

