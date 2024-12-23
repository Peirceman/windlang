package main

var keywords = map[string]TokenType{
	"const":     TTConst,
	"var":       TTVar,
	"fn":        TTFn,
	"return":    TTReturn,
	"type":      TTType,
	"struct":    TTStruct,
	"interface": TTInterface,
	"if":        TTIf,
	"elif":      TTElif,
	"else":      TTElse,
	"true":      TTTrue,
	"false":     TTFalse,
	"while":     TTWhile,
}

var binaryCharset = map[rune]int{'0': 1, '1': 1}

var octalCharset = map[rune]int{
	'0': 0,
	'1': 1,
	'2': 2,
	'3': 3,
	'4': 4,
	'5': 5,
	'6': 6,
	'7': 7,
}

var decimalCharset = map[rune]int{
	'0': 0,
	'1': 1,
	'2': 2,
	'3': 3,
	'4': 4,
	'5': 5,
	'6': 6,
	'7': 7,
	'8': 8,
	'9': 9,
}

var hexCharset = map[rune]int{
	'0': 0x0,
	'1': 0x1,
	'2': 0x2,
	'3': 0x3,
	'4': 0x4,
	'5': 0x5,
	'6': 0x6,
	'7': 0x7,
	'8': 0x8,
	'9': 0x9,
	'A': 0xA,
	'B': 0xB,
	'C': 0xC,
	'D': 0xD,
	'E': 0xE,
	'F': 0xF,
	'a': 0xa,
	'b': 0xb,
	'c': 0xc,
	'd': 0xd,
	'e': 0xe,
	'f': 0xf,
}
