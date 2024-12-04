package main

import (
	"io"
)

type Generator struct {
	out io.Writer
	in  AstNode
}
