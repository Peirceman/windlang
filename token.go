package main

import "strconv"

type TokenType int

type Token struct {
	typ     TokenType
	loc     Location
	literal string
}

const (
	TTIllegal TokenType = iota
	TTEOF
	TTConst
	TTVar
	TTFn
	TTReturn
	TTType
	TTStruct
	TTInterface
	TTIf
	TTElif
	TTElse
	TTTrue
	TTFalse
	TTIdentifier
	TTNumber
	TTComment
	TTPlus
	TTMinus
	TTMul
	TTDiv
	TTAssign
	TTPlusAsign
	TTMinusAsign
	TTMulAsign
	TTDivAsign
	TTExclam
	TTEqual
	TTNotEqual
	TTPeriod
	TTComma
	TTColon
	TTSemiColon
	TTLBrace
	TTRBrace
	TTLSquare
	TTRSquare
	TTLSquirly
	TTRSquirly
	TTLength
)

func (tokTyp TokenType) String() string {
	if TTLength != 39 {
		panic("TokenType enum length changed")
	}

	switch tokTyp {
	case TTIllegal:
		return "TTIllegal"
	case TTEOF:
		return "TTEOF"
	case TTConst:
		return "TTConst"
	case TTVar:
		return "TTVar"
	case TTFn:
		return "TTFn"
	case TTReturn:
		return "TTReturn"
	case TTType:
		return "TTType"
	case TTStruct:
		return "TTStruct"
	case TTInterface:
		return "TTInterface"
	case TTIf:
		return "TTIf"
	case TTElif:
		return "TTElif"
	case TTElse:
		return "TTElse"
	case TTTrue:
		return "TTTrue"
	case TTFalse:
		return "TTFalse"
	case TTIdentifier:
		return "TTIdentifier"
	case TTNumber:
		return "TTNumber"
	case TTPlus:
		return "TTPlus"
	case TTMinus:
		return "TTMinus"
	case TTMul:
		return "TTMul"
	case TTDiv:
		return "TTDiv"
	case TTPlusAsign:
		return "TTPlusAsign"
	case TTMinusAsign:
		return "TTMinusAsign"
	case TTMulAsign:
		return "TTMulAsign"
	case TTDivAsign:
		return "TTDivAsign"
	case TTAssign:
		return "TTAsign"
	case TTExclam:
		return "TTExclam"
	case TTEqual:
		return "TTEqual"
	case TTNotEqual:
		return "TTNotEqual"
	case TTPeriod:
		return "TTPeriod"
	case TTComma:
		return "TTComma"
	case TTColon:
		return "TTColon"
	case TTSemiColon:
		return "TTSemiColon"
	case TTComment:
		return "TTComment"
	case TTLBrace:
		return "TTLBrace"
	case TTRBrace:
		return "TTRBrace"
	case TTLSquare:
		return "TTLSquare"
	case TTRSquare:
		return "TTRSquare"
	case TTLSquirly:
		return "TTLSquirly"
	case TTRSquirly:
		return "TTRSquirly"
	default:
		panic("unkown TokenType")
	}
}

func (t *Token) String() string {
	return t.loc.String() + " " + t.typ.String() + " `" + t.literal + "`"
}

type Location struct {
	file      string
	line, col int
}

func (l Location) String() string {
	return l.file + ":" + strconv.Itoa(l.line) + ":" + strconv.Itoa(l.col) + ":"
}
