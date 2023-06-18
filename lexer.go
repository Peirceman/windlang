package main

import (
	"bufio"
	"io"
	"os"
	"strings"
	"unicode"
)

type NextRuneFunc func() (r rune, eof bool)

type Lexer struct {
	nextRuneFunc NextRuneFunc
	preekRune    rune
	peekEOF      bool
	hasPreekRune bool
	curLoc       Location
}

func ReaderRuneReader(r *bufio.Reader) NextRuneFunc {
	return func() (rune, bool) {
		r, _, err := r.ReadRune()
		if err == io.EOF {
			return r, true
		}

		if err != nil {
			panic(err)
		}

		return r, false
	}
}

func StringRuneReader(str string) NextRuneFunc {
	return ReaderRuneReader(bufio.NewReader(strings.NewReader(str)))
}

func LexerFromFilename(filename string) *Lexer {
	f, err := os.Open(filename)
	if err != nil {
		panic(err)
	}

	r := bufio.NewReader(f)

	return &Lexer{
		nextRuneFunc: ReaderRuneReader(r),
		curLoc:       Location{filename, 1, 1},
	}
}

func LexerFromString(str string) *Lexer {
	return &Lexer{
		nextRuneFunc: StringRuneReader(str),
		curLoc:       Location{"", 1, 1},
	}
}

func (l *Lexer) peekRune() (rune, bool) {
	if l.hasPreekRune {
		return l.preekRune, l.peekEOF
	}

	l.preekRune, l.peekEOF = l.nextRuneFunc()
	l.hasPreekRune = true

	return l.preekRune, l.peekEOF
}

func (l *Lexer) nextRune() (r rune, eof bool) {
	r, eof = l.peekRune()
	l.hasPreekRune = false
	l.curLoc.col++

	if eof {
		return
	}

	if r == '\n' {
		l.curLoc.col = 1
		l.curLoc.line++
	}

	return
}

func (l *Lexer) skipWhitespace() {
	for r, eof := l.peekRune(); !eof && unicode.IsSpace(r); r, eof = l.peekRune() {
		l.nextRune()
	}
}

func (l *Lexer) nextToken() *Token {
	l.skipWhitespace()

	r, eof := l.peekRune()
	if eof {
		return &Token{
			typ:     TTEOF,
			loc:     l.curLoc,
			literal: "",
		}
	}

	if TTLength != 38 {
		panic("TokenType enum length changed")
	}

	if unicode.IsLetter(r) {
		return l.parseLetter()
	}

	if unicode.IsDigit(r) {
		return l.parseDigit()
	}

	// symbols

	tok := &Token{loc: l.curLoc}
	l.nextRune()
	switch r {
	case '+':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.literal = "+="
			tok.typ = TTPlusAsign
		} else {
			tok.literal = "+"
			tok.typ = TTPlus
		}

	case '-':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.literal = "-="
			tok.typ = TTMinusAsign
		} else {
			tok.literal = "-"
			tok.typ = TTMinus
		}

	case '*':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.literal = "*="
			tok.typ = TTMulAsign
		} else {
			tok.literal = "*"
			tok.typ = TTMul
		}

	case '/':
		r, eof = l.peekRune()
		tok.literal = "/"
		tok.typ = TTDiv
		if eof {
			break
		}

		switch r {
		case '=':
			l.nextRune()
			tok.literal = "/="
			tok.typ = TTDivAsign
		case '/':
			l.nextRune()
			literal := strings.Builder{}
			literal.WriteString("//")

			for r, eof := l.peekRune(); !eof && r != '\n'; r, eof = l.peekRune() {
				l.nextRune()
				literal.WriteRune(r)
			}

			tok.literal = literal.String()
			tok.typ = TTComment
		case '*':
			l.nextRune()
			literal := strings.Builder{}
			literal.WriteString("/*")

			for r, eof := l.peekRune(); !eof; r, eof = l.peekRune() {
				l.nextRune()
				literal.WriteRune(r)
				if r != '*' {
					continue
				}

				r, eof := l.peekRune()
				if eof {
					panic(l.curLoc.String() + " unterminated comment")
				}

				if r == '/' {
					l.nextRune()
					literal.WriteRune('/')
					goto ok
				}
			}

			panic(l.curLoc.String() + "unterminated comment")

		ok:
			tok.literal = literal.String()
			tok.typ = TTComment
		}

	case '!':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.literal = "!="
			tok.typ = TTNotEqual
		} else {
			tok.literal = "!"
			tok.typ = TTExclam
		}

	case '=':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.literal = "=="
			tok.typ = TTEqual
		} else {
			tok.literal = "="
			tok.typ = TTAssign
		}

	case '.':
		tok.literal = "."
		tok.typ = TTPeriod

	case ',':
		tok.literal = ","
		tok.typ = TTComma

	case ':':
		tok.literal = ":"
		tok.typ = TTColon

	case ';':
		tok.literal = ";"
		tok.typ = TTSemiColon

	case '(':
		tok.literal = "("
		tok.typ = TTLBrace

	case ')':
		tok.literal = ")"
		tok.typ = TTRBrace

	case '[':
		tok.literal = "["
		tok.typ = TTLSquare

	case ']':
		tok.literal = "]"
		tok.typ = TTRSquare

	case '{':
		tok.literal = "{"
		tok.typ = TTLSquirly

	case '}':
		tok.literal = "}"
		tok.typ = TTRSquirly

	default:
		panic(l.curLoc.String() + " illegal char `" + string(r) + "`")
	}

	return tok
}

func (l *Lexer) parseLetter() *Token {
	tokenText := strings.Builder{}
	tok := &Token{}
	tok.loc = l.curLoc

	for r, eof := l.peekRune(); !eof && (unicode.IsLetter(r) || unicode.IsDigit(r)); r, eof = l.peekRune() {
		l.nextRune()
		tokenText.WriteRune(r)
	}

	tok.literal = tokenText.String()

	if typ, isKeyword := keywords[tok.literal]; isKeyword {
		tok.typ = typ
	} else {
		tok.typ = TTIdentifier
	}

	return tok
}

func (l *Lexer) parseDigit() *Token {
	tokenText := strings.Builder{}
	tok := &Token{
		loc: l.curLoc,
		typ: TTNumber,
	}
	charset := decimalCharset

	r, eof := l.nextRune()
	if eof {
		// people don't like goto???
		goto end
	}

	tokenText.WriteRune(r)

	if r == '0' {
		r, eof = l.peekRune()
		if eof {
			goto end
		}

		switch r {
		case 'b':
			charset = binaryCharset
		case 'o':
			charset = octalCharset
		case 'x':
			charset = hexCharset
		default:
			goto parseRest
		}

		tokenText.WriteRune(r)
		l.nextRune()
	}

parseRest:
	for r, eof = l.peekRune(); !eof && charset[r]; r, eof = l.peekRune() {
		l.nextRune()
		tokenText.WriteRune(r)
	}

end:
	tok.literal = tokenText.String()

	return tok
}
