package main

import (
	"fmt"
	"os"
	"strings"
	"windlang/ast"
	"windlang/lexer"
)

func TypeCheck(block ast.CodeBlockNode) (ok bool) {
	ok = true

	for _, statement := range block.Statements {
		switch node := statement.(type) {
		case ast.VarNode:
			if node.Value == nil {
				break
			}

			if !typeCheckExpression(node.Value, node.Typ) {
				ok = false
				break
			}

			ok = checkTypes(node.Loc(), node.Typ, node.Value.ReturnType()) && ok

		case ast.FuncNode:
			if !typeCheckCodeBlock(node.Body, node.ReturnType) {
				ok = false
			}

		default:
			panic("unreachable")
		}
	}

	return ok
}

// expected can be nil if type does not matter
func typeCheckExpression(expression ast.Expression, expected ast.Type) bool {
	return true
}

func typeCheckCodeBlock(block ast.CodeBlockNode, returnType ast.Type) (ok bool) {
	ok = true

	for _, statement := range block.Statements {
		switch node := statement.(type) {
		case ast.VarNode:
			if node.Value == nil {
				break
			}

			if !typeCheckExpression(node.Value, node.Typ) {
				ok = false
				break
			}

			ok = checkTypes(node.Loc(), node.Typ, node.Value.ReturnType()) && ok

		case ast.ReturnNode:
			if returnType == ast.TypeVoid {
				if node.Expr != nil {
					printError(node.Loc(), "no return values expected")
					ok = false
				}

				break
			}

			if node.Expr == nil {
				printError(node.Loc(), "return value expected")
				ok = false
				break
			}

			ok = checkTypes(node.Loc(), returnType, node.Expr.ReturnType()) && ok

		case ast.CodeBlockNode:
			ok = typeCheckCodeBlock(node, returnType) && ok

		case ast.IfChain:
			for i, statement := range node.Statements {
				if i < len(node.Conditions) {
					ok = typeCheckExpression(node.Conditions[i], nil) && ok

					if node.Conditions[i].ReturnType().Kind() != ast.KindBool {
						printError(node.Loc(), "boolean expression expected in if statement")
						ok = false
					}
				}

				ok = typeCheckCodeBlock(statement, returnType) && ok
			}

		case ast.WhileNode:
			ok = typeCheckExpression(node.Condition, nil) && ok

			if node.Condition.ReturnType().Kind() != ast.KindBool {
				printError(node.Loc(), "boolean expression expected in while-loop")
				ok = false
			}

			ok = typeCheckCodeBlock(node.Body, returnType) && ok

		case ast.ExpressionNode:
			ok = typeCheckExpression(node.Expr, nil) && ok

		default:
			panic("unreachable")
		}
	}

	return ok
}

func checkTypes(loc lexer.Location, expected, got ast.Type) bool {
	if !ast.EqualTypes(expected, got) {
		printError(
			loc,
			"cannot use expression of type", typeToString(got),
			"as expression of type", typeToString(expected),
		)
		return false
	}

	return true
}

func typeToString(typ ast.Type) string {
	sb := strings.Builder{}
	typeToStringSb(typ, &sb)
	return sb.String()
}

func typeToStringSb(typ ast.Type, sb *strings.Builder) {
	for {
		if typ.Name() != "" {
			sb.WriteString(string(typ.Name()))
			break
		}

		switch cur := typ.(type) {
		case ast.SimpleType:
			panic("unreachable, simpletype should always have a name")

		case ast.PointerType:
			sb.WriteByte('&')
			typ = cur.Inner

		case ast.StructType:
			sb.WriteString("struct{")

			for i, field := range cur.Fields {
				sb.WriteString(string(field.Name))
				sb.WriteString(": ")
				typeToStringSb(field.Typ, sb)

				if i < len(cur.Fields)-1 {
					sb.WriteString(", ")
				}
			}

			sb.WriteByte('}')

		case ast.ArrayType:
			sb.WriteByte('[')
			sb.WriteByte(']')
			typ = cur.Inner

		default:
			panic("unreachable")
		}
	}

}

func printError(loc lexer.Location, message ...any) {
	fmt.Fprint(os.Stderr, loc.String()+"ERROR: ")
	fmt.Fprintln(os.Stderr, message...)
}
