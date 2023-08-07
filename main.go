package main

import (
	"fmt"
	"os"
)

func main() {

	fileName := os.Args[1]

	lex := LexerFromFilename(fileName)
	par := &Parser{lex}
	for line, eof := par.ParseTopLevel(); !eof; line, eof = par.ParseTopLevel() {
		if line == nil {
			continue
		}

		fmt.Printf("%v\n", line.String())
	}
}
