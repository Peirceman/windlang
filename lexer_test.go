package main

import (
	"testing"
)

func TestLexer_1(t *testing.T) {
	code := `/* this is a block comment */
const αConst: int = 69;
var a: int = 0x1A4;

a = aConst * 3;

// line comment
if a == 50 {
	io.print(50);
} elif a != 30 {
	print(!true);
} else {
	print(30);
}

fn get(ints: int[], idx: int): int {
	return ints[idx]
}`

	i := 0
	expected := []Token{
		{TTComment, Location{"", 1, 1}, "/* this is a block comment */"},
		{TTConst, Location{"", 2, 1}, "const"},
		{TTIdentifier, Location{"", 2, 7}, "αConst"},
		{TTColon, Location{"", 2, 13}, ":"},
		{TTIdentifier, Location{"", 2, 15}, "int"},
		{TTAssign, Location{"", 2, 19}, "="},
		{TTNumber, Location{"", 2, 21}, "69"},
		{TTSemiColon, Location{"", 2, 23}, ";"},
		{TTVar, Location{"", 3, 1}, "var"},
		{TTIdentifier, Location{"", 3, 5}, "a"},
		{TTColon, Location{"", 3, 6}, ":"},
		{TTIdentifier, Location{"", 3, 8}, "int"},
		{TTAssign, Location{"", 3, 12}, "="},
		{TTNumber, Location{"", 3, 14}, "0x1A4"},
		{TTSemiColon, Location{"", 3, 19}, ";"},
		{TTIdentifier, Location{"", 5, 1}, "a"},
		{TTAssign, Location{"", 5, 3}, "="},
		{TTIdentifier, Location{"", 5, 5}, "aConst"},
		{TTMul, Location{"", 5, 12}, "*"},
		{TTNumber, Location{"", 5, 14}, "3"},
		{TTSemiColon, Location{"", 5, 15}, ";"},
		{TTComment, Location{"", 7, 1}, "// line comment"},
		{TTIf, Location{"", 8, 1}, "if"},
		{TTIdentifier, Location{"", 8, 4}, "a"},
		{TTEqual, Location{"", 8, 6}, "=="},
		{TTNumber, Location{"", 8, 9}, "50"},
		{TTLSquirly, Location{"", 8, 12}, "{"},
		{TTIdentifier, Location{"", 9, 2}, "io"},
		{TTPeriod, Location{"", 9, 4}, "."},
		{TTIdentifier, Location{"", 9, 5}, "print"},
		{TTLBrace, Location{"", 9, 10}, "("},
		{TTNumber, Location{"", 9, 11}, "50"},
		{TTRBrace, Location{"", 9, 13}, ")"},
		{TTSemiColon, Location{"", 9, 14}, ";"},
		{TTRSquirly, Location{"", 10, 1}, "}"},
		{TTElif, Location{"", 10, 3}, "elif"},
		{TTIdentifier, Location{"", 10, 8}, "a"},
		{TTNotEqual, Location{"", 10, 10}, "!="},
		{TTNumber, Location{"", 10, 13}, "30"},
		{TTLSquirly, Location{"", 10, 16}, "{"},
		{TTIdentifier, Location{"", 11, 2}, "print"},
		{TTLBrace, Location{"", 11, 7}, "("},
		{TTExclam, Location{"", 11, 8}, "!"},
		{TTTrue, Location{"", 11, 9}, "true"},
		{TTRBrace, Location{"", 11, 13}, ")"},
		{TTSemiColon, Location{"", 11, 14}, ";"},
		{TTRSquirly, Location{"", 12, 1}, "}"},
		{TTElse, Location{"", 12, 3}, "else"},
		{TTLSquirly, Location{"", 12, 8}, "{"},
		{TTIdentifier, Location{"", 13, 2}, "print"},
		{TTLBrace, Location{"", 13, 7}, "("},
		{TTNumber, Location{"", 13, 8}, "30"},
		{TTRBrace, Location{"", 13, 10}, ")"},
		{TTSemiColon, Location{"", 13, 11}, ";"},
		{TTRSquirly, Location{"", 14, 1}, "}"},
		{TTFn, Location{"", 16, 1}, "fn"},
		{TTIdentifier, Location{"", 16, 4}, "get"},
		{TTLBrace, Location{"", 16, 7}, "("},
		{TTIdentifier, Location{"", 16, 8}, "ints"},
		{TTColon, Location{"", 16, 12}, ":"},
		{TTIdentifier, Location{"", 16, 14}, "int"},
		{TTLSquare, Location{"", 16, 17}, "["},
		{TTRSquare, Location{"", 16, 18}, "]"},
		{TTComma, Location{"", 16, 19}, ","},
		{TTIdentifier, Location{"", 16, 21}, "idx"},
		{TTColon, Location{"", 16, 24}, ":"},
		{TTIdentifier, Location{"", 16, 26}, "int"},
		{TTRBrace, Location{"", 16, 29}, ")"},
		{TTColon, Location{"", 16, 30}, ":"},
		{TTIdentifier, Location{"", 16, 32}, "int"},
		{TTLSquirly, Location{"", 16, 36}, "{"},
		{TTReturn, Location{"", 17, 2}, "return"},
		{TTIdentifier, Location{"", 17, 9}, "ints"},
		{TTLSquare, Location{"", 17, 13}, "["},
		{TTIdentifier, Location{"", 17, 14}, "idx"},
		{TTRSquare, Location{"", 17, 17}, "]"},
		{TTRSquirly, Location{"", 18, 1}, "}"},
		{TTEOF, Location{"", 18, 2}, ""},
	}

	lex := LexerFromString(code)
	exitNext := false
	for tok := lex.nextToken(); !exitNext; tok, i = lex.nextToken(), i+1 {
		if *tok != expected[i] {
			t.Logf("expected `%s`, but got `%s`", expected[i].String(), tok.String())
			t.Fail()
		}

		exitNext = tok.typ == TTEOF
	}
}

func TestLexer_2(t *testing.T) {
	code := `+-/
+=
-=
*=
/=`

	i := 0
	expected := []Token{
		{TTPlus, Location{"", 1, 1}, "+"},
		{TTMinus, Location{"", 1, 2}, "-"},
		{TTDiv, Location{"", 1, 3}, "/"},
		{TTPlusAsign, Location{"", 2, 1}, "+="},
		{TTMinusAsign, Location{"", 3, 1}, "-="},
		{TTMulAsign, Location{"", 4, 1}, "*="},
		{TTDivAsign, Location{"", 5, 1}, "/="},
		{TTEOF, Location{"", 5, 3}, ""},
	}

	lex := LexerFromString(code)
	exitNext := false
	for tok := lex.nextToken(); !exitNext; tok, i = lex.nextToken(), i+1 {
		if *tok != expected[i] {
			t.Logf("expected `%s`, but got `%s`", expected[i].String(), tok.String())
			t.Fail()
		}

		exitNext = tok.typ == TTEOF
	}
}
