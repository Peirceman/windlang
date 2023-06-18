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
}

var binaryCharset = map[rune]bool{'0': true, '1': true}

var octalCharset = map[rune]bool{
	'0': true,
	'1': true,
	'2': true,
	'3': true,
	'4': true,
	'5': true,
	'6': true,
	'7': true,
}

var decimalCharset = map[rune]bool{
	'0': true,
	'1': true,
	'2': true,
	'3': true,
	'4': true,
	'5': true,
	'6': true,
	'7': true,
	'8': true,
	'9': true,
}

var hexCharset = map[rune]bool{
	'0': true,
	'1': true,
	'2': true,
	'3': true,
	'4': true,
	'5': true,
	'6': true,
	'7': true,
	'8': true,
	'9': true,
	'A': true,
	'B': true,
	'C': true,
	'D': true,
	'E': true,
	'F': true,
	'a': true,
	'b': true,
	'c': true,
	'd': true,
	'e': true,
	'f': true,
}
