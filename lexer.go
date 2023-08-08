package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
	"unicode"
)

type NextRuneFunc func() (r rune, eof bool)

type Lexer struct {
	nextRuneFunc  NextRuneFunc
	peekedRune    rune
	peekEOF       bool
	hasPeekedRune bool

	curLoc Location
	idx    int
	tokens []Token
}

func ReaderRuneReader(r *bufio.Reader) NextRuneFunc {
	return func() (rune, bool) {
		r, _, err := r.ReadRune()
		if err == io.EOF {
			return r, true
		}

		if err != nil {
			panic(err) // TODO: better error handling
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
		panic(err) // TODO: better error handling
	}

	r := bufio.NewReader(f)

	return NewLexer(ReaderRuneReader(r), Location{filename, 1, 1})
}

func LexerFromString(str string) *Lexer {
	return NewLexer(StringRuneReader(str), Location{"--", 1, 1})
}

func NewLexer(nextRuneFunc NextRuneFunc, loc Location) *Lexer {
	lex := &Lexer{
		nextRuneFunc: nextRuneFunc,
		curLoc:       loc,
	}

	lex.readAllTokens()

	return lex
}

func (l *Lexer) peekRune() (rune, bool) {
	if l.hasPeekedRune {
		return l.peekedRune, l.peekEOF
	}

	l.peekedRune, l.peekEOF = l.nextRuneFunc()
	l.hasPeekedRune = true

	return l.peekedRune, l.peekEOF
}

func (l *Lexer) nextRune() (r rune, eof bool) {
	r, eof = l.peekRune()
	l.hasPeekedRune = false

	if eof {
		return
	}

	l.curLoc.col++

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

func (l *Lexer) readAllTokens() {
	tok := l.nextToken()
	for ; tok.typ != TTEOF; tok = l.nextToken() {
		l.tokens = append(l.tokens, *tok)
	}

	l.tokens = append(l.tokens, *tok)
	l.curLoc = l.tokens[0].loc
}

func (l *Lexer) PeekToken() *Token {
	return &l.tokens[l.idx]
}

func (l *Lexer) NextToken() *Token {
	tok := &l.tokens[l.idx]
	l.idx++
	l.curLoc = l.tokens[l.idx].loc
	return tok
}

func (l *Lexer) Reset() {
	l.idx = 0
	l.curLoc = l.tokens[0].loc
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

	if TTCount != 59 {
		panic("TokenType enum length changed: " + strconv.Itoa(int(TTCount)))
	}

	if unicode.IsLetter(r) {
		return l.readAfterLetter()
	}

	if unicode.IsDigit(r) {
		return l.readAfterDigit()
	}

	// symbols

	tok := &Token{loc: l.curLoc}
	l.nextRune()

	switch r {
	case '+':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.literal = "+="
			tok.typ = TTPlusAssign
		} else {
			tok.literal = "+"
			tok.typ = TTPlus
		}

	case '-':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.literal = "-="
			tok.typ = TTDashAssign
		} else {
			tok.literal = "-"
			tok.typ = TTDash
		}

	case '*':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.literal = "*="
			tok.typ = TTStarAssign
		} else {
			tok.literal = "*"
			tok.typ = TTStar
		}

	case '/':
		tok.literal, tok.typ = l.readAfterSlash()
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

	case '|':
		r, eof = l.peekRune()
		tok.literal = "|"
		tok.typ = TTBar
		if eof {
			break
		}

		if r == '|' {
			l.nextRune()
			tok.literal = "||"
			tok.typ = TTOr
		} else if r == '=' {
			l.nextRune()
			tok.literal = "|="
			tok.typ = TTBarAssign
		}

	case '&':
		r, eof = l.peekRune()
		tok.literal = "&"
		tok.typ = TTAmp
		if eof {
			break
		}

		if r == '&' {
			l.nextRune()
			tok.literal = "&&"
			tok.typ = TTAnd
		} else if r == '=' {
			l.nextRune()
			tok.literal = "&="
			tok.typ = TTAmpAssign
		}

	case '^':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.literal = "^="
			tok.typ = TTCaretAssign
		} else {
			tok.literal = "^"
			tok.typ = TTCaret
		}

	case '~':
		tok.literal = "~"
		tok.typ = TTTilde

	case '>':
		// posibilities: > >> >>= >=
		tok.literal = ">"
		tok.typ = TTGt

		r, eof = l.peekRune()
		if eof {
			break
		}

		if r == '>' {
			l.nextRune()
			tok.literal = ">>"
			tok.typ = TTShr

			r, eof = l.peekRune()

			if !eof && r == '=' {
				l.nextRune()
				tok.literal = ">>="
				tok.typ = TTShrAssign
				break
			}
		} else if r == '=' {
			l.nextRune()
			tok.literal = ">="
			tok.typ = TTGtEq
		}

	case '<':
		// posibilities: < << <<= <=
		tok.literal = "<"
		tok.typ = TTLt

		r, eof = l.peekRune()
		if eof {
			break
		}

		if r == '<' {
			l.nextRune()
			tok.literal = "<<"
			tok.typ = TTShl

			r, eof = l.peekRune()

			if !eof && r == '=' {
				l.nextRune()
				tok.literal = "<<="
				tok.typ = TTShlAssign
				break
			}
		} else if r == '=' {
			l.nextRune()
			tok.literal = "<="
			tok.typ = TTLtEq
		}

	case '\'':
		tok.typ = TTChar
		tok.literal, tok.extraInfo = l.readCharLitteral()

	case '"':
		tok.typ = TTString
		tok.literal, tok.extraInfo = l.readStringLitteral()

	default:
		tok.literal = string(r)
		tok.typ = TTIllegal
	}

	return tok
}

func (l *Lexer) readStringLitteral() (string, string) {
	text := strings.Builder{}
	litteral := strings.Builder{}
	litteral.WriteRune('"')
	r, eof := l.peekRune()

	for ; !eof && r != '"'; r, eof = l.peekRune() {
		if r == '\n' {
			panic(l.curLoc.String() + " unterminated string litteral") // TODO: better error handling
		}
		l.nextRune()

		if r != '\\' {
			text.WriteRune(r)
			litteral.WriteRune(r)
			continue
		}

		litteral.WriteRune('\\')

		r, eof = l.peekRune()

		if eof {
			l.nextRune()
			panic(l.curLoc.String() + " unterminated string litteral") // TODO: better error handling
		}

		if r == '\n' {
			panic(l.curLoc.String() + " unterminated string litteral") // TODO: better error handling
		}

		l.nextRune()

		switch r {
		case '"':
			text.WriteRune(r)
			litteral.WriteRune(r)
		case '\\':
			text.WriteRune(r)
			litteral.WriteRune(r)
		case 'n':
			text.WriteRune('\n')
			litteral.WriteRune(r)
		case 'r':
			text.WriteRune('\r')
			litteral.WriteRune(r)
		case 't':
			text.WriteRune('\t')
			litteral.WriteRune(r)

		case 'u':
			// Read the unicode codepoint
			litteral.WriteRune(r)
			var codepoint rune
			for i := 0; i < 4; i++ {
				r, eof = l.nextRune()
				if eof {
					panic(l.curLoc.String() + " unterminated char litteral") // TODO: better error handling
				}

				val, contains := hexCharset[r]
				if !contains {
					panic(l.curLoc.String() + " illegal char in escape sequence") // TODO: better error handling
				}

				litteral.WriteRune(r)
				codepoint = codepoint*16 + rune(val)
			}

			text.WriteRune(codepoint)

		case 'x':
			var char rune
			// Check for byte escape sequence

			for i := 0; i < 2; i++ {
				r, eof = l.nextRune()
				if eof {
					panic(l.curLoc.String() + " unterminated char litteral") // TODO: better error handling
				}

				val, contains := hexCharset[r]
				if !contains {
					panic(l.curLoc.String() + " illegal char in escape sequence") // TODO: better error handling
				}

				char = char*16 + rune(val)
				litteral.WriteRune(r)
			}

			text.WriteRune(char)

		default:
			panic(l.curLoc.String() + " invalid char in escape equence") // TODO: better error handling
		}

	}

	if eof {
		panic(l.curLoc.String() + " unterminated string litteral" + string(r)) // TODO: better error handling
	}

	l.nextRune()
	litteral.WriteRune('"')

	return litteral.String(), text.String()
}

func (l *Lexer) readCharLitteral() (string, rune) {
	r, eof := l.peekRune()
	if eof {
		l.nextRune()
		panic(l.curLoc.String() + " unterminated char litteral") // TODO: better error handling
	}

	if r == '\n' {
		panic(l.curLoc.String() + " unterminated char litteral") // TODO: better error handling
	}

	l.nextRune()

	if r == '\'' {
		panic(l.curLoc.String() + " empty char litteral") // TODO: better error handling
	}

	checkClosing := func() {
		r, eof := l.peekRune()
		if eof || r != '\'' {
			panic(l.curLoc.String() + " unterminated char litteral") // TODO: better error handling
		}
		l.nextRune()
	}

	// Handle escape sequences
	if r == '\\' {
		// Read the escape sequence character
		r, eof = l.peekRune()
		if eof {
			l.nextRune()
			panic(l.curLoc.String() + " unterminated char litteral") // TODO: better error handling
		}

		if r == '\n' {
			panic(l.curLoc.String() + " unterminated char litteral") // TODO: better error handling
		}

		l.nextRune()

		// Check for allowed escape sequences
		switch r {
		case '\'':
			checkClosing()
			return "'''", '\''
		case '\\':
			checkClosing()
			return "'\\\\'", '\\'
		case 'n':
			checkClosing()
			return "'\\n'", '\n'
		case 'r':
			checkClosing()
			return "'\\r'", '\r'
		case 't':
			checkClosing()
			return "'\\t'", '\t'

		case 'u':
			// Read the unicode codepoint
			litteral := strings.Builder{}
			litteral.WriteString("'\\u")
			var codepoint rune

			for i := 0; i < 4; i++ {
				r, eof = l.nextRune()
				if eof {
					panic(l.curLoc.String() + " unterminated char litteral") // TODO: better error handling
				}

				val, contains := hexCharset[r]
				if !contains {
					panic(l.curLoc.String() + " illegal char in escape sequence") // TODO: better error handling
				}

				codepoint = codepoint*16 + rune(val)
				litteral.WriteRune(r)
			}

			checkClosing()
			litteral.WriteRune('\'')
			return litteral.String(), codepoint

		case 'x':
			// Check for byte escape sequence
			litteral := strings.Builder{}
			litteral.WriteString("'\\x")
			var char rune

			for i := 0; i < 2; i++ {
				r, eof = l.nextRune()
				if eof {
					panic(l.curLoc.String() + " unterminated char litteral") // TODO: better error handling
				}

				val, contains := hexCharset[r]
				if !contains {
					panic(l.curLoc.String() + " illegal char `" + string(r) + "` in escape sequence") // TODO: better error handling
				}

				char = char*16 + rune(val)
				litteral.WriteRune(r)
			}

			checkClosing()
			litteral.WriteRune('\'')
			return litteral.String(), char

		default:
			panic(l.curLoc.String() + " invalid char `" + string(r) + "` in escape equence") // TODO: better error handling
		}
	}

	checkClosing()
	return "'" + string(r) + "'", r
}

func (l *Lexer) readAfterLetter() *Token {
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

func (l *Lexer) readAfterDigit() *Token {
	tokenText := strings.Builder{}
	tok := &Token{
		loc: l.curLoc,
	}

	var charsetName string = "decimal"
	var charset map[rune]int = decimalCharset
	var contains bool
	isDecimal := true

	r, _ := l.nextRune() // cannot be eof, because this function would otherwise not be called
	tokenText.WriteRune(r)

	if r == '0' {
		r, eof := l.peekRune()
		if eof {
			tok.literal = tokenText.String()
			return tok
		}

		switch r {
		case 'b':
			charsetName = "binary"
			charset = binaryCharset
			isDecimal = false
		case 'o':
			charsetName = "octal"
			charset = octalCharset
			isDecimal = false
		case 'x':
			charsetName = "hex"
			charset = hexCharset
			isDecimal = false
		default:
			if _, contains = charset[r]; !contains {
				tok.literal = tokenText.String()
				return tok
			}
		}

		tokenText.WriteRune(r)
		l.nextRune()
	}

	r, eof := l.peekRune()
	_, contains = charset[r]
	hasDecimalPoint := false
	for !eof && (contains || r == '.' && isDecimal) {
		l.nextRune()
		tokenText.WriteRune(r)
		if r == '.' {
			if hasDecimalPoint {
				panic(l.curLoc.String() + ": Error: to many decimal points in number")
			}

			hasDecimalPoint = true
		}

		r, eof = l.peekRune()
		_, contains = charset[r]
	}

	if _, contains := decimalCharset[r]; !eof && contains {
		panic(fmt.Sprintf(l.curLoc.String()+" illegal digit `"+string(r)+"` in %s literal", charsetName)) // TODO: better error handling
	}

	if hasDecimalPoint {
		tok.typ = TTFloat
	} else {
		tok.typ = TTInt
	}

	tok.literal = tokenText.String()
	return tok
}

func (l *Lexer) readAfterSlash() (lit string, typ TokenType) {
	r, eof := l.peekRune()
	lit = "/"
	typ = TTSlash

	if eof {
		return
	}

	switch r {
	case '=':
		l.nextRune()
		lit = "/="
		typ = TTSlashAssign
	case '/':
		l.nextRune()
		literal := strings.Builder{}
		literal.WriteString("//")

		for r, eof := l.peekRune(); !eof && r != '\n'; r, eof = l.peekRune() {
			l.nextRune()
			literal.WriteRune(r)
		}

		lit = literal.String()
		typ = TTComment
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
				panic(l.curLoc.String() + " unterminated comment") // TODO: better error handling
			}

			if r == '/' {
				l.nextRune()
				literal.WriteRune('/')
				lit = literal.String()
				typ = TTComment
				return
			}
		}

		panic(l.curLoc.String() + "unterminated comment") // TODO: better error handling
	}

	return
}
