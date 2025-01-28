package lexer

import (
	"strconv"
)

type TokenType uint

type Location struct {
	Filename  string
	Line, Col int
}

type Token struct {
	Typ       TokenType
	Loc       Location
	Literal   string
	ExtraInfo any // optional
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
	TTWhile
	TTFalse

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
	return l.Filename + ":" + strconv.Itoa(l.Line) + ":" + strconv.Itoa(l.Col) + ":"
}

func (l Location) SyntaxString() string {
	return "{\"" + l.Filename + "\", " + strconv.Itoa(l.Line) + ", " + strconv.Itoa(l.Col) + "}"
}

func (t *Token) String() string {
	return t.Loc.String() + " " + t.Typ.String() + " `" + t.Literal + "`"
}

func (t *Token) SyntaxString() string {
	return "{" + t.Typ.String() + ", " + t.Loc.SyntaxString() + ", \"" + t.Literal + "\"}"
}


