package main

import (
	"testing"
)

func TestLexer_1(t *testing.T) {
	code := `/* this is a block comment
 * it can go on multiple lines
 */
const αConst: int = 69;
var a: int = 0x1A4;

a = αConst * 3 + 1;

// line comment
if a == 50 {
	io.print(50);
} else if a != 30 {
	print(!true);
} else {
	print('\x41');
}

fn get(ints: int[], idx: int): int {
	return ints[idx];
}

var str: string = "\u0009string\n\tmulti line";
`

	expected := []Token{
		{typ: TTConst, loc: Location{"--", 4, 1}, literal: "const"},
		{typ: TTIdentifier, loc: Location{"--", 4, 7}, literal: "αConst"},
		{typ: TTColon, loc: Location{"--", 4, 13}, literal: ":"},
		{typ: TTIdentifier, loc: Location{"--", 4, 15}, literal: "int"},
		{typ: TTAssign, loc: Location{"--", 4, 19}, literal: "="},
		{typ: TTInt, loc: Location{"--", 4, 21}, literal: "69"},
		{typ: TTSemiColon, loc: Location{"--", 4, 23}, literal: ";"},
		{typ: TTVar, loc: Location{"--", 5, 1}, literal: "var"},
		{typ: TTIdentifier, loc: Location{"--", 5, 5}, literal: "a"},
		{typ: TTColon, loc: Location{"--", 5, 6}, literal: ":"},
		{typ: TTIdentifier, loc: Location{"--", 5, 8}, literal: "int"},
		{typ: TTAssign, loc: Location{"--", 5, 12}, literal: "="},
		{typ: TTInt, loc: Location{"--", 5, 14}, literal: "0x1A4"},
		{typ: TTSemiColon, loc: Location{"--", 5, 19}, literal: ";"},
		{typ: TTIdentifier, loc: Location{"--", 7, 1}, literal: "a"},
		{typ: TTAssign, loc: Location{"--", 7, 3}, literal: "="},
		{typ: TTIdentifier, loc: Location{"--", 7, 5}, literal: "αConst"},
		{typ: TTStar, loc: Location{"--", 7, 12}, literal: "*"},
		{typ: TTInt, loc: Location{"--", 7, 14}, literal: "3"},
		{typ: TTPlus, loc: Location{"--", 7, 16}, literal: "+"},
		{typ: TTInt, loc: Location{"--", 7, 18}, literal: "1"},
		{typ: TTSemiColon, loc: Location{"--", 7, 19}, literal: ";"},
		{typ: TTIf, loc: Location{"--", 10, 1}, literal: "if"},
		{typ: TTIdentifier, loc: Location{"--", 10, 4}, literal: "a"},
		{typ: TTEqual, loc: Location{"--", 10, 6}, literal: "=="},
		{typ: TTInt, loc: Location{"--", 10, 9}, literal: "50"},
		{typ: TTLSquirly, loc: Location{"--", 10, 12}, literal: "{"},
		{typ: TTIdentifier, loc: Location{"--", 11, 2}, literal: "io"},
		{typ: TTPeriod, loc: Location{"--", 11, 4}, literal: "."},
		{typ: TTIdentifier, loc: Location{"--", 11, 5}, literal: "print"},
		{typ: TTLBrace, loc: Location{"--", 11, 10}, literal: "("},
		{typ: TTInt, loc: Location{"--", 11, 11}, literal: "50"},
		{typ: TTRBrace, loc: Location{"--", 11, 13}, literal: ")"},
		{typ: TTSemiColon, loc: Location{"--", 11, 14}, literal: ";"},
		{typ: TTRSquirly, loc: Location{"--", 12, 1}, literal: "}"},
		{typ: TTElse, loc: Location{"--", 12, 3}, literal: "else"},
		{typ: TTIf, loc: Location{"--", 12, 8}, literal: "if"},
		{typ: TTIdentifier, loc: Location{"--", 12, 13}, literal: "a"},
		{typ: TTNotEqual, loc: Location{"--", 12, 15}, literal: "!="},
		{typ: TTInt, loc: Location{"--", 12, 18}, literal: "30"},
		{typ: TTLSquirly, loc: Location{"--", 12, 21}, literal: "{"},
		{typ: TTIdentifier, loc: Location{"--", 13, 2}, literal: "print"},
		{typ: TTLBrace, loc: Location{"--", 13, 7}, literal: "("},
		{typ: TTExclam, loc: Location{"--", 13, 8}, literal: "!"},
		{typ: TTTrue, loc: Location{"--", 13, 9}, literal: "true"},
		{typ: TTRBrace, loc: Location{"--", 13, 13}, literal: ")"},
		{typ: TTSemiColon, loc: Location{"--", 13, 14}, literal: ";"},
		{typ: TTRSquirly, loc: Location{"--", 14, 1}, literal: "}"},
		{typ: TTElse, loc: Location{"--", 14, 3}, literal: "else"},
		{typ: TTLSquirly, loc: Location{"--", 14, 8}, literal: "{"},
		{typ: TTIdentifier, loc: Location{"--", 15, 2}, literal: "print"},
		{typ: TTLBrace, loc: Location{"--", 15, 7}, literal: "("},
		{typ: TTChar, loc: Location{"--", 15, 8}, literal: "'\\x41'", extraInfo: 'A'},
		{typ: TTRBrace, loc: Location{"--", 15, 14}, literal: ")"},
		{typ: TTSemiColon, loc: Location{"--", 15, 15}, literal: ";"},
		{typ: TTRSquirly, loc: Location{"--", 16, 1}, literal: "}"},
		{typ: TTFn, loc: Location{"--", 18, 1}, literal: "fn"},
		{typ: TTIdentifier, loc: Location{"--", 18, 4}, literal: "get"},
		{typ: TTLBrace, loc: Location{"--", 18, 7}, literal: "("},
		{typ: TTIdentifier, loc: Location{"--", 18, 8}, literal: "ints"},
		{typ: TTColon, loc: Location{"--", 18, 12}, literal: ":"},
		{typ: TTIdentifier, loc: Location{"--", 18, 14}, literal: "int"},
		{typ: TTLSquare, loc: Location{"--", 18, 17}, literal: "["},
		{typ: TTRSquare, loc: Location{"--", 18, 18}, literal: "]"},
		{typ: TTComma, loc: Location{"--", 18, 19}, literal: ","},
		{typ: TTIdentifier, loc: Location{"--", 18, 21}, literal: "idx"},
		{typ: TTColon, loc: Location{"--", 18, 24}, literal: ":"},
		{typ: TTIdentifier, loc: Location{"--", 18, 26}, literal: "int"},
		{typ: TTRBrace, loc: Location{"--", 18, 29}, literal: ")"},
		{typ: TTColon, loc: Location{"--", 18, 30}, literal: ":"},
		{typ: TTIdentifier, loc: Location{"--", 18, 32}, literal: "int"},
		{typ: TTLSquirly, loc: Location{"--", 18, 36}, literal: "{"},
		{typ: TTReturn, loc: Location{"--", 19, 2}, literal: "return"},
		{typ: TTIdentifier, loc: Location{"--", 19, 9}, literal: "ints"},
		{typ: TTLSquare, loc: Location{"--", 19, 13}, literal: "["},
		{typ: TTIdentifier, loc: Location{"--", 19, 14}, literal: "idx"},
		{typ: TTRSquare, loc: Location{"--", 19, 17}, literal: "]"},
		{typ: TTSemiColon, loc: Location{"--", 19, 18}, literal: ";"},
		{typ: TTRSquirly, loc: Location{"--", 20, 1}, literal: "}"},
		{typ: TTVar, loc: Location{"--", 22, 1}, literal: "var"},
		{typ: TTIdentifier, loc: Location{"--", 22, 5}, literal: "str"},
		{typ: TTColon, loc: Location{"--", 22, 8}, literal: ":"},
		{typ: TTIdentifier, loc: Location{"--", 22, 10}, literal: "string"},
		{typ: TTAssign, loc: Location{"--", 22, 17}, literal: "="},
		{typ: TTString, loc: Location{"--", 22, 19}, literal: "\"\\u0009string\\n\\tmulti line\"", extraInfo: "\tstring\n\tmulti line"},
		{typ: TTSemiColon, loc: Location{"--", 22, 47}, literal: ";"},
		{typ: TTEOF, loc: Location{"--", 23, 1}, literal: ""},
	}

	lex := LexerFromString(code)
	exitNext := false
	for tok, i := lex.NextToken(), 0; !exitNext; tok, i = lex.NextToken(), i+1 {
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

	expected := []Token{
		{typ: TTPlus, loc: Location{"--", 1, 1}, literal: "+"},
		{typ: TTDash, loc: Location{"--", 1, 2}, literal: "-"},
		{typ: TTSlash, loc: Location{"--", 1, 3}, literal: "/"},
		{typ: TTPlusAssign, loc: Location{"--", 2, 1}, literal: "+="},
		{typ: TTDashAssign, loc: Location{"--", 3, 1}, literal: "-="},
		{typ: TTStarAssign, loc: Location{"--", 4, 1}, literal: "*="},
		{typ: TTSlashAssign, loc: Location{"--", 5, 1}, literal: "/="},
		{typ: TTEOF, loc: Location{"--", 5, 3}, literal: ""},
	}

	lex := LexerFromString(code)
	exitNext := false
	for tok, i := lex.NextToken(), 0; !exitNext; tok, i = lex.NextToken(), i+1 {
		exitNext = tok.typ == TTEOF
		if *tok != expected[i] {
			t.Logf("expected `%s`, but got `%s`", expected[i].String(), tok.String())
			t.Fail()
		}
	}
}
