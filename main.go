package main

import (
	"fmt"
	"os"
)

func main() {
	fileName := os.Args[1]

	lex := LexerFromFilename(fileName)
	exitNext := false
	for tok := lex.nextToken(); !exitNext; tok = lex.nextToken() {
		fmt.Println(tok)
		exitNext = tok.typ == TTEOF
	}
}
