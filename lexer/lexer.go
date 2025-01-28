package lexer

import (
	"bufio"
	"errors"
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

	l.curLoc.Col++

	if r == '\n' {
		l.curLoc.Col = 1
		l.curLoc.Line++
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
	for ; tok.Typ != TTEOF; tok = l.nextToken() {
		if tok.Typ != TTComment {
			l.tokens = append(l.tokens, *tok)
		}
	}

	l.tokens = append(l.tokens, *tok)
	l.curLoc = l.tokens[0].Loc
}

func (l *Lexer) PeekToken() *Token {
	return &l.tokens[l.idx]
}

func (l *Lexer) NextToken() *Token {
	tok := &l.tokens[l.idx]
	if l.idx < len(l.tokens)-1 {
		l.idx++
		l.curLoc = l.tokens[l.idx].Loc
	}

	return tok
}

func (l *Lexer) Reset() {
	l.idx = 0
	l.curLoc = l.tokens[0].Loc
}

// Seek sets the index for which token it is on depending on
// the offset and the whence as described in [io.Seek]. On
// error, the error is returned and the token is not changed.
func (l *Lexer) Seek(offset int64, whence int) (int64, error) {
	oldIdx := l.idx

	switch whence {
	case io.SeekStart:
		l.idx = int(offset)
	case io.SeekEnd:
		l.idx = len(l.tokens) - int(offset)
	case io.SeekCurrent:
		l.idx += int(offset)
	default:
		return 0, errors.New("ERROR: invalid whence")
	}

	if l.idx < 0 || l.idx >= len(l.tokens) {
		l.idx = oldIdx
		return 0, errors.New("ERROR: seek out of bounds")
	}

	l.curLoc = l.tokens[l.idx].Loc

	return int64(l.idx), nil
}

func (l *Lexer) nextToken() *Token {
	l.skipWhitespace()

	r, eof := l.peekRune()
	if eof {
		return &Token{
			Typ:     TTEOF,
			Loc:     l.curLoc,
			Literal: "",
		}
	}

	if TTCount != 60 {
		panic("TokenType enum length changed: " + strconv.Itoa(int(TTCount)))
	}

	if unicode.IsLetter(r) || r == '_' {
		return l.readAfterLetter()
	}

	if unicode.IsDigit(r) {
		return l.readAfterDigit()
	}

	// symbols

	tok := &Token{Loc: l.curLoc}
	l.nextRune()

	switch r {
	case '+':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.Literal = "+="
			tok.Typ = TTPlusAssign
		} else {
			tok.Literal = "+"
			tok.Typ = TTPlus
		}

	case '-':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.Literal = "-="
			tok.Typ = TTDashAssign
		} else {
			tok.Literal = "-"
			tok.Typ = TTDash
		}

	case '*':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.Literal = "*="
			tok.Typ = TTStarAssign
		} else {
			tok.Literal = "*"
			tok.Typ = TTStar
		}

	case '/':
		tok.Literal, tok.Typ = l.readAfterSlash()

	case '%':
		tok.Literal = "%"
		tok.Typ = TTPercent

	case '!':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.Literal = "!="
			tok.Typ = TTNotEqual
		} else {
			tok.Literal = "!"
			tok.Typ = TTExclam
		}

	case '=':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.Literal = "=="
			tok.Typ = TTEqual
		} else {
			tok.Literal = "="
			tok.Typ = TTAssign
		}

	case '.':
		tok.Literal = "."
		tok.Typ = TTPeriod

	case ',':
		tok.Literal = ","
		tok.Typ = TTComma

	case ':':
		tok.Literal = ":"
		tok.Typ = TTColon

	case ';':
		tok.Literal = ";"
		tok.Typ = TTSemiColon

	case '(':
		tok.Literal = "("
		tok.Typ = TTLBrace

	case ')':
		tok.Literal = ")"
		tok.Typ = TTRBrace

	case '[':
		tok.Literal = "["
		tok.Typ = TTLSquare

	case ']':
		tok.Literal = "]"
		tok.Typ = TTRSquare

	case '{':
		tok.Literal = "{"
		tok.Typ = TTLSquirly

	case '}':
		tok.Literal = "}"
		tok.Typ = TTRSquirly

	case '|':
		r, eof = l.peekRune()
		tok.Literal = "|"
		tok.Typ = TTBar
		if eof {
			break
		}

		if r == '|' {
			l.nextRune()
			tok.Literal = "||"
			tok.Typ = TTOr
		} else if r == '=' {
			l.nextRune()
			tok.Literal = "|="
			tok.Typ = TTBarAssign
		}

	case '&':
		r, eof = l.peekRune()
		tok.Literal = "&"
		tok.Typ = TTAmp
		if eof {
			break
		}

		if r == '&' {
			l.nextRune()
			tok.Literal = "&&"
			tok.Typ = TTAnd
		} else if r == '=' {
			l.nextRune()
			tok.Literal = "&="
			tok.Typ = TTAmpAssign
		}

	case '^':
		if r, eof = l.peekRune(); !eof && r == '=' {
			l.nextRune()
			tok.Literal = "^="
			tok.Typ = TTCaretAssign
		} else {
			tok.Literal = "^"
			tok.Typ = TTCaret
		}

	case '~':
		tok.Literal = "~"
		tok.Typ = TTTilde

	case '>':
		// posibilities: > >> >>= >=
		tok.Literal = ">"
		tok.Typ = TTGt

		r, eof = l.peekRune()
		if eof {
			break
		}

		if r == '>' {
			l.nextRune()
			tok.Literal = ">>"
			tok.Typ = TTShr

			r, eof = l.peekRune()

			if !eof && r == '=' {
				l.nextRune()
				tok.Literal = ">>="
				tok.Typ = TTShrAssign
				break
			}
		} else if r == '=' {
			l.nextRune()
			tok.Literal = ">="
			tok.Typ = TTGtEq
		}

	case '<':
		// posibilities: < << <<= <=
		tok.Literal = "<"
		tok.Typ = TTLt

		r, eof = l.peekRune()
		if eof {
			break
		}

		if r == '<' {
			l.nextRune()
			tok.Literal = "<<"
			tok.Typ = TTShl

			r, eof = l.peekRune()

			if !eof && r == '=' {
				l.nextRune()
				tok.Literal = "<<="
				tok.Typ = TTShlAssign
				break
			}
		} else if r == '=' {
			l.nextRune()
			tok.Literal = "<="
			tok.Typ = TTLtEq
		}

	case '\'':
		tok.Typ = TTChar
		tok.Literal, tok.ExtraInfo = l.readCharLitteral()

	case '"':
		tok.Typ = TTString
		tok.Literal, tok.ExtraInfo = l.readStringLitteral()

	default:
		tok.Literal = string(r)
		tok.Typ = TTIllegal
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

	// Guess I wanted to do function objects?
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
	tok.Loc = l.curLoc

	for r, eof := l.peekRune(); !eof && (unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_'); r, eof = l.peekRune() {
		l.nextRune()
		tokenText.WriteRune(r)
	}

	tok.Literal = tokenText.String()

	if typ, isKeyword := keywords[tok.Literal]; isKeyword {
		tok.Typ = typ
	} else {
		tok.Typ = TTIdentifier
	}

	return tok
}

func (l *Lexer) readAfterDigit() *Token {
	tokenText := strings.Builder{}
	tok := &Token{
		Loc: l.curLoc,
	}

	var charsetName string = "decimal"
	var charset map[rune]int = decimalCharset
	var contains bool
	var intVal int
	base := 10

	r, _ := l.nextRune() // cannot be eof, because this function would otherwise not be called
	tokenText.WriteRune(r)
	hasDecimalPoint := false

	if r == '0' {
		r, eof := l.peekRune()
		if eof {
			tok.Literal = tokenText.String()
			return tok
		}

		switch r {
		case 'b':
			charsetName = "binary"
			charset = binaryCharset
			base = 2
		case 'o':
			charsetName = "octal"
			charset = octalCharset
			base = 8
		case 'x':
			charsetName = "hex"
			charset = hexCharset
			base = 16
		case '.':
			hasDecimalPoint = true
		default:
			if _, contains = decimalCharset[r]; !contains {
				tok.Literal = tokenText.String()
				tok.Typ = TTInt
				tok.ExtraInfo = int(0)
				return tok
			}
		}

		tokenText.WriteRune(r)
		l.nextRune()
	} else {
		intVal = charset[r]
	}

	r, eof := l.peekRune()
	_, contains = charset[r]
	for !eof && (contains || (r == '.' && base == 10)) {
		l.nextRune()
		tokenText.WriteRune(r)
		if r == '.' {
			if hasDecimalPoint {
				panic(l.curLoc.String() + ": Error: to many decimal points in number")
			}

			hasDecimalPoint = true
		}

		if !hasDecimalPoint {
			intVal = intVal*base + charset[r]
		}

		r, eof = l.peekRune()
		_, contains = charset[r]
	}

	if _, contains := charset[r]; !eof && contains {
		panic(fmt.Sprintf(l.curLoc.String()+" illegal digit `"+string(r)+"` in %s literal", charsetName)) // TODO: better error handling
	}

	tok.Literal = tokenText.String()

	if hasDecimalPoint {
		tok.Typ = TTFloat
		tok.ExtraInfo, _ = strconv.ParseFloat(tok.Literal, 64)
	} else {
		tok.Typ = TTInt
		tok.ExtraInfo = intVal
	}

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
