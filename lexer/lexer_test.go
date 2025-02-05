package lexer

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
		{Typ: TTConst, Loc: Location{"--", 4, 1}, Literal: "const"},
		{Typ: TTIdentifier, Loc: Location{"--", 4, 7}, Literal: "αConst"},
		{Typ: TTColon, Loc: Location{"--", 4, 13}, Literal: ":"},
		{Typ: TTIdentifier, Loc: Location{"--", 4, 15}, Literal: "int"},
		{Typ: TTAssign, Loc: Location{"--", 4, 19}, Literal: "="},
		{Typ: TTInt, Loc: Location{"--", 4, 21}, Literal: "69", ExtraInfo: 69},
		{Typ: TTSemiColon, Loc: Location{"--", 4, 23}, Literal: ";"},
		{Typ: TTVar, Loc: Location{"--", 5, 1}, Literal: "var"},
		{Typ: TTIdentifier, Loc: Location{"--", 5, 5}, Literal: "a"},
		{Typ: TTColon, Loc: Location{"--", 5, 6}, Literal: ":"},
		{Typ: TTIdentifier, Loc: Location{"--", 5, 8}, Literal: "int"},
		{Typ: TTAssign, Loc: Location{"--", 5, 12}, Literal: "="},
		{Typ: TTInt, Loc: Location{"--", 5, 14}, Literal: "0x1A4", ExtraInfo: 0x1A4},
		{Typ: TTSemiColon, Loc: Location{"--", 5, 19}, Literal: ";"},
		{Typ: TTIdentifier, Loc: Location{"--", 7, 1}, Literal: "a"},
		{Typ: TTAssign, Loc: Location{"--", 7, 3}, Literal: "="},
		{Typ: TTIdentifier, Loc: Location{"--", 7, 5}, Literal: "αConst"},
		{Typ: TTStar, Loc: Location{"--", 7, 12}, Literal: "*"},
		{Typ: TTInt, Loc: Location{"--", 7, 14}, Literal: "3", ExtraInfo: 3},
		{Typ: TTPlus, Loc: Location{"--", 7, 16}, Literal: "+"},
		{Typ: TTInt, Loc: Location{"--", 7, 18}, Literal: "1", ExtraInfo: 1},
		{Typ: TTSemiColon, Loc: Location{"--", 7, 19}, Literal: ";"},
		{Typ: TTIf, Loc: Location{"--", 10, 1}, Literal: "if"},
		{Typ: TTIdentifier, Loc: Location{"--", 10, 4}, Literal: "a"},
		{Typ: TTEqual, Loc: Location{"--", 10, 6}, Literal: "=="},
		{Typ: TTInt, Loc: Location{"--", 10, 9}, Literal: "50", ExtraInfo: 50},
		{Typ: TTLSquirly, Loc: Location{"--", 10, 12}, Literal: "{"},
		{Typ: TTIdentifier, Loc: Location{"--", 11, 2}, Literal: "io"},
		{Typ: TTPeriod, Loc: Location{"--", 11, 4}, Literal: "."},
		{Typ: TTIdentifier, Loc: Location{"--", 11, 5}, Literal: "print"},
		{Typ: TTLBrace, Loc: Location{"--", 11, 10}, Literal: "("},
		{Typ: TTInt, Loc: Location{"--", 11, 11}, Literal: "50", ExtraInfo: 50},
		{Typ: TTRBrace, Loc: Location{"--", 11, 13}, Literal: ")"},
		{Typ: TTSemiColon, Loc: Location{"--", 11, 14}, Literal: ";"},
		{Typ: TTRSquirly, Loc: Location{"--", 12, 1}, Literal: "}"},
		{Typ: TTElse, Loc: Location{"--", 12, 3}, Literal: "else"},
		{Typ: TTIf, Loc: Location{"--", 12, 8}, Literal: "if"},
		{Typ: TTIdentifier, Loc: Location{"--", 12, 11}, Literal: "a"},
		{Typ: TTNotEqual, Loc: Location{"--", 12, 13}, Literal: "!="},
		{Typ: TTInt, Loc: Location{"--", 12, 16}, Literal: "30", ExtraInfo: 30},
		{Typ: TTLSquirly, Loc: Location{"--", 12, 19}, Literal: "{"},
		{Typ: TTIdentifier, Loc: Location{"--", 13, 2}, Literal: "print"},
		{Typ: TTLBrace, Loc: Location{"--", 13, 7}, Literal: "("},
		{Typ: TTExclam, Loc: Location{"--", 13, 8}, Literal: "!"},
		{Typ: TTTrue, Loc: Location{"--", 13, 9}, Literal: "true"},
		{Typ: TTRBrace, Loc: Location{"--", 13, 13}, Literal: ")"},
		{Typ: TTSemiColon, Loc: Location{"--", 13, 14}, Literal: ";"},
		{Typ: TTRSquirly, Loc: Location{"--", 14, 1}, Literal: "}"},
		{Typ: TTElse, Loc: Location{"--", 14, 3}, Literal: "else"},
		{Typ: TTLSquirly, Loc: Location{"--", 14, 8}, Literal: "{"},
		{Typ: TTIdentifier, Loc: Location{"--", 15, 2}, Literal: "print"},
		{Typ: TTLBrace, Loc: Location{"--", 15, 7}, Literal: "("},
		{Typ: TTChar, Loc: Location{"--", 15, 8}, Literal: "'\\x41'", ExtraInfo: 'A'},
		{Typ: TTRBrace, Loc: Location{"--", 15, 14}, Literal: ")"},
		{Typ: TTSemiColon, Loc: Location{"--", 15, 15}, Literal: ";"},
		{Typ: TTRSquirly, Loc: Location{"--", 16, 1}, Literal: "}"},
		{Typ: TTFn, Loc: Location{"--", 18, 1}, Literal: "fn"},
		{Typ: TTIdentifier, Loc: Location{"--", 18, 4}, Literal: "get"},
		{Typ: TTLBrace, Loc: Location{"--", 18, 7}, Literal: "("},
		{Typ: TTIdentifier, Loc: Location{"--", 18, 8}, Literal: "ints"},
		{Typ: TTColon, Loc: Location{"--", 18, 12}, Literal: ":"},
		{Typ: TTIdentifier, Loc: Location{"--", 18, 14}, Literal: "int"},
		{Typ: TTLSquare, Loc: Location{"--", 18, 17}, Literal: "["},
		{Typ: TTRSquare, Loc: Location{"--", 18, 18}, Literal: "]"},
		{Typ: TTComma, Loc: Location{"--", 18, 19}, Literal: ","},
		{Typ: TTIdentifier, Loc: Location{"--", 18, 21}, Literal: "idx"},
		{Typ: TTColon, Loc: Location{"--", 18, 24}, Literal: ":"},
		{Typ: TTIdentifier, Loc: Location{"--", 18, 26}, Literal: "int"},
		{Typ: TTRBrace, Loc: Location{"--", 18, 29}, Literal: ")"},
		{Typ: TTColon, Loc: Location{"--", 18, 30}, Literal: ":"},
		{Typ: TTIdentifier, Loc: Location{"--", 18, 32}, Literal: "int"},
		{Typ: TTLSquirly, Loc: Location{"--", 18, 36}, Literal: "{"},
		{Typ: TTReturn, Loc: Location{"--", 19, 2}, Literal: "return"},
		{Typ: TTIdentifier, Loc: Location{"--", 19, 9}, Literal: "ints"},
		{Typ: TTLSquare, Loc: Location{"--", 19, 13}, Literal: "["},
		{Typ: TTIdentifier, Loc: Location{"--", 19, 14}, Literal: "idx"},
		{Typ: TTRSquare, Loc: Location{"--", 19, 17}, Literal: "]"},
		{Typ: TTSemiColon, Loc: Location{"--", 19, 18}, Literal: ";"},
		{Typ: TTRSquirly, Loc: Location{"--", 20, 1}, Literal: "}"},
		{Typ: TTVar, Loc: Location{"--", 22, 1}, Literal: "var"},
		{Typ: TTIdentifier, Loc: Location{"--", 22, 5}, Literal: "str"},
		{Typ: TTColon, Loc: Location{"--", 22, 8}, Literal: ":"},
		{Typ: TTIdentifier, Loc: Location{"--", 22, 10}, Literal: "string"},
		{Typ: TTAssign, Loc: Location{"--", 22, 17}, Literal: "="},
		{Typ: TTString, Loc: Location{"--", 22, 19}, Literal: "\"\\u0009string\\n\\tmulti line\"", ExtraInfo: "\tstring\n\tmulti line"},
		{Typ: TTSemiColon, Loc: Location{"--", 22, 47}, Literal: ";"},
		{Typ: TTEOF, Loc: Location{"--", 23, 1}, Literal: ""},
	}

	lex := LexerFromString(code)
	exitNext := false
	for tok, i := lex.NextToken(), 0; !exitNext; tok, i = lex.NextToken(), i+1 {
		if *tok != expected[i] {
			t.Logf("expected `%#v`, but got `%#v`", expected[i], tok)
			t.Fail()
		}

		exitNext = tok.Typ == TTEOF
	}
}

func TestLexer_2(t *testing.T) {
	code := `+-/
+=
-=
*=
/=`

	expected := []Token{
		{Typ: TTPlus, Loc: Location{"--", 1, 1}, Literal: "+"},
		{Typ: TTDash, Loc: Location{"--", 1, 2}, Literal: "-"},
		{Typ: TTSlash, Loc: Location{"--", 1, 3}, Literal: "/"},
		{Typ: TTPlusAssign, Loc: Location{"--", 2, 1}, Literal: "+="},
		{Typ: TTDashAssign, Loc: Location{"--", 3, 1}, Literal: "-="},
		{Typ: TTStarAssign, Loc: Location{"--", 4, 1}, Literal: "*="},
		{Typ: TTSlashAssign, Loc: Location{"--", 5, 1}, Literal: "/="},
		{Typ: TTEOF, Loc: Location{"--", 5, 3}, Literal: ""},
	}

	lex := LexerFromString(code)
	exitNext := false
	for tok, i := lex.NextToken(), 0; !exitNext; tok, i = lex.NextToken(), i+1 {
		exitNext = tok.Typ == TTEOF
		if *tok != expected[i] {
			t.Logf("expected `%#v`, but got `%#v`", expected[i], tok)
			t.Fail()
		}
	}
}
