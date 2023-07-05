package main

import (
	"fmt"
	"os"
)

func main() {
	fileName := os.Args[1]

	lex := LexerFromFilename(fileName)
	par := &Parser{lex}
	for line := par.parseFunctionBody(); line != nil; line = par.parseFunctionBody() {
		fmt.Println(line.String())
	}
	lex = LexerFromFilename(fileName)
}
