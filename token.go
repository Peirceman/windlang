package main

import (
	"strconv"
	"windlang/ast"
)

type TokenType uint

type Location struct {
	filename  string
	line, col int
}

type Token struct {
	typ       TokenType
	loc       Location
	literal   string
	extraInfo any // optional
}

const (
	TTIllegal TokenType = iota
	TTEOF

	// keywords
	TTConst
	TTVar
	TTFn
	TTReturn
	TTType
	TTStruct
	TTInterface
	TTIf
	TTElse
	TTTrue
	TTFalse
	TTWhile

	// symbols
	TTAssign

	TTPlus
	TTDash
	TTStar
	TTSlash
	TTPercent

	TTPlusAssign
	TTDashAssign
	TTStarAssign
	TTSlashAssign

	TTAmp
	TTBar
	TTCaret
	TTTilde

	TTAnd
	TTOr

	TTGt
	TTLt
	TTGtEq
	TTLtEq

	TTShr
	TTShl

	TTAmpAssign
	TTBarAssign
	TTCaretAssign
	TTShrAssign
	TTShlAssign

	TTExclam
	TTEqual
	TTNotEqual

	TTColon
	TTSemiColon
	TTComma
	TTPeriod

	TTLBrace
	TTRBrace
	TTLSquare
	TTRSquare
	TTLSquirly
	TTRSquirly

	// text

	TTIdentifier
	TTInt
	TTFloat
	TTComment

	TTChar
	TTString

	TTCount
)

func (t TokenType) String() string {
	if TTCount != 60 {
		panic("Token Type count changed")
	}

	switch t {
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
	case TTElse:
		return "TTElse"
	case TTTrue:
		return "TTTrue"
	case TTFalse:
		return "TTFalse"
	case TTWhile:
		return "TTWhile"
	case TTAssign:
		return "TTAssign"
	case TTPlus:
		return "TTPlus"
	case TTDash:
		return "TTDash"
	case TTStar:
		return "TTStar"
	case TTSlash:
		return "TTSlash"
	case TTPercent:
		return "TTPercent"
	case TTPlusAssign:
		return "TTPlusAssign"
	case TTDashAssign:
		return "TTDashAssign"
	case TTStarAssign:
		return "TTStarAssign"
	case TTSlashAssign:
		return "TTSlashAssign"
	case TTAmp:
		return "TTAmp"
	case TTBar:
		return "TTBar"
	case TTCaret:
		return "TTCaret"
	case TTTilde:
		return "TTTilde"
	case TTAnd:
		return "TTAnd"
	case TTOr:
		return "TTOr"
	case TTGt:
		return "TTGt"
	case TTLt:
		return "TTLt"
	case TTGtEq:
		return "TTGtEq"
	case TTLtEq:
		return "TTLtEq"
	case TTShr:
		return "TTShr"
	case TTShl:
		return "TTShl"
	case TTAmpAssign:
		return "TTAmpAssign"
	case TTBarAssign:
		return "TTBarAssign"
	case TTCaretAssign:
		return "TTCaretAssign"
	case TTShrAssign:
		return "TTShrAssign"
	case TTShlAssign:
		return "TTShlAssign"
	case TTExclam:
		return "TTExclam"
	case TTEqual:
		return "TTEqual"
	case TTNotEqual:
		return "TTNotEqual"
	case TTColon:
		return "TTColon"
	case TTSemiColon:
		return "TTSemiColon"
	case TTComma:
		return "TTComma"
	case TTPeriod:
		return "TTPeriod"
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
	case TTIdentifier:
		return "TTIdentifier"
	case TTInt:
		return "TTInt"
	case TTFloat:
		return "TTFloat"
	case TTComment:
		return "TTComment"
	case TTChar:
		return "TTChar"
	case TTString:
		return "TTString"
	default:
		panic("unkown type")
	}
}

func (t TokenType) SymbolToString() string {
	if TTCount != 60 {
		panic("Token Type count changed")
	}

	switch t {
	case TTAssign:
		return "="
	case TTPlus:
		return "+"
	case TTDash:
		return "-"
	case TTStar:
		return "*"
	case TTSlash:
		return "/"
	case TTPercent:
		return "%"
	case TTPlusAssign:
		return "+="
	case TTDashAssign:
		return "-="
	case TTStarAssign:
		return "*="
	case TTSlashAssign:
		return "/="
	case TTAmp:
		return "&"
	case TTBar:
		return "|"
	case TTCaret:
		return "^"
	case TTTilde:
		return "~"
	case TTAnd:
		return "&&"
	case TTOr:
		return "||"
	case TTGt:
		return ">"
	case TTLt:
		return "<"
	case TTGtEq:
		return ">="
	case TTLtEq:
		return "<="
	case TTShr:
		return ">>"
	case TTShl:
		return "<<"
	case TTAmpAssign:
		return "&="
	case TTBarAssign:
		return "|="
	case TTCaretAssign:
		return "^="
	case TTShrAssign:
		return ">>="
	case TTShlAssign:
		return "<<="
	case TTExclam:
		return "!"
	case TTEqual:
		return "=="
	case TTNotEqual:
		return "!="
	case TTColon:
		return ":"
	case TTSemiColon:
		return ";"
	case TTComma:
		return ","
	case TTPeriod:
		return "."
	case TTLBrace:
		return "("
	case TTRBrace:
		return ")"
	case TTLSquare:
		return "["
	case TTRSquare:
		return "]"
	case TTLSquirly:
		return "{"
	case TTRSquirly:
		return "}"
	}

	if t >= TTCount {
		panic("unkown type")
	}

	panic("Not a symbol")
}

func (l Location) String() string {
	return l.filename + ":" + strconv.Itoa(l.line) + ":" + strconv.Itoa(l.col) + ":"
}

func (l Location) SyntaxString() string {
	return "{\"" + l.filename + "\", " + strconv.Itoa(l.line) + ", " + strconv.Itoa(l.col) + "}"
}

func (t *Token) String() string {
	return t.loc.String() + " " + t.typ.String() + " `" + t.literal + "`"
}

func (t *Token) SyntaxString() string {
	return "{" + t.typ.String() + ", " + t.loc.SyntaxString() + ", \"" + t.literal + "\"}"
}

// ToBinOp converts the token type to it's
// binary opperation, returns -1 if it isn't a binary op
func (t TokenType) ToBinOp() ast.BinaryOp {
	if ast.BOCount != 28 {
		panic("Binary opperation enum length changed")
	}

	if TTCount != 60 {
		panic("Token type enum length changed")
	}

	switch t {
	case TTAssign:
		return ast.BOAssign
	case TTPlus:
		return ast.BOPlus
	case TTDash:
		return ast.BOMinus
	case TTStar:
		return ast.BOMul
	case TTSlash:
		return ast.BODiv
	case TTPercent:
		return ast.BOMod
	case TTPlusAssign:
		return ast.BOPlusAssign
	case TTDashAssign:
		return ast.BODashAssign
	case TTStarAssign:
		return ast.BOStarAssign
	case TTSlashAssign:
		return ast.BOSlashAssign
	case TTAmp:
		return ast.BOBinAnd
	case TTBar:
		return ast.BOBinOr
	case TTCaret:
		return ast.BOBinXor
	case TTAnd:
		return ast.BOBoolAnd
	case TTOr:
		return ast.BOBoolOr
	case TTGt:
		return ast.BOGt
	case TTLt:
		return ast.BOLt
	case TTGtEq:
		return ast.BOGtEq
	case TTLtEq:
		return ast.BOLtEq
	case TTShr:
		return ast.BOShr
	case TTShl:
		return ast.BOShl
	case TTAmpAssign:
		return ast.BOAndAssign
	case TTBarAssign:
		return ast.BOOrAssign
	case TTCaretAssign:
		return ast.BOXorAssign
	case TTShrAssign:
		return ast.BOShrAssign
	case TTShlAssign:
		return ast.BOShlAssign
	case TTEqual:
		return ast.BOEquals
	case TTNotEqual:
		return ast.BONotEqual
	}

	return -1
}

func (t TokenType) ToUnOp() ast.UnaryOp {
	if ast.UOCount != 6 {
		panic("Unary opperation enum length changed")
	}

	if TTCount != 60 {
		panic("Token type enum length changed")
	}

	switch t {
	case TTPlus:
		return ast.UOPlus
	case TTDash:
		return ast.UONegative
	case TTExclam:
		return ast.UOBoolNot
	case TTTilde:
		return ast.UOBinNot
	case TTAmp:
		return ast.UORef
	case TTStar:
		return ast.UODeref
	}

	return -1
}
