package main

import (
	"fmt"
	"os"
	"strings"
	"windlang/ast"
	"windlang/lexer"
)

func TypeCheckGlobal(block ast.CodeBlockNode) (ok bool) {
	ok = true

	for _, statement := range block.Statements {
		switch node := statement.(type) {
		case ast.VarNode:
			if node.Value == nil {
				break
			}

			if typeCheckExpression(node.Value, node.Typ) {
				ok = checkTypes(node.Loc(), node.Typ, node.Value.ReturnType()) && ok
			} else {
				ok = false
			}

		case ast.FuncNode:
			ok = typeCheckCodeBlock(node.Body, node.ReturnType) && ok

		default:
			panic("unreachable")
		}
	}

	return
}

func typeCheckCodeBlock(block ast.CodeBlockNode, returnType ast.Type) (ok bool) {
	ok = true

	for _, statement := range block.Statements {
		switch node := statement.(type) {
		case ast.VarNode:
			if node.Value == nil {
				break
			}

			if typeCheckExpression(node.Value, node.Typ) {
				ok = checkTypes(node.Loc(), node.Typ, node.Value.ReturnType()) && ok
			} else {
				ok = false
			}

		case ast.ReturnNode:
			if returnType == ast.TypeVoid {
				if node.Expr != nil {
					printErrorln(node.Loc(), "no return values expected")
					ok = false
				}

				break
			}

			if node.Expr == nil {
				printErrorln(node.Loc(), "return value expected")
				ok = false
				break
			}

			ok = typeCheckExpression(node.Expr, returnType) && ok

		case ast.CodeBlockNode:
			ok = typeCheckCodeBlock(node, returnType) && ok

		case ast.IfChain:
			for i, statement := range node.Statements {
				if i < len(node.Conditions) {
					ok = typeCheckExpression(node.Conditions[i], nil) && ok

					if node.Conditions[i].ReturnType().Kind() != ast.KindBool {
						printErrorln(node.Loc(), "boolean expression expected in if statement")
						ok = false
					}
				}

				ok = typeCheckCodeBlock(statement, returnType) && ok
			}

		case ast.WhileNode:
			ok = typeCheckExpression(node.Condition, nil) && ok

			if node.Condition.ReturnType().Kind() != ast.KindBool {
				printErrorln(node.Loc(), "boolean expression expected in while-loop")
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

// expected can be nil if type does not matter
func typeCheckExpression(expression ast.Expression, expected ast.Type) bool {
	switch expression := expression.(type) {
	case ast.IntLit,
		ast.FloatLit,
		ast.StrLit,
		ast.CharLit,
		ast.BoolLit,
		ast.Var:
		return true

	case ast.Func:
		panic("unreachable")

	case ast.FuncCall:
		if len(expression.Fun.Args) < len(expression.Args) {
			printErrorln(
				expression.Loc(), "to many arguments to function, expected",
				len(expression.Fun.Args), "got", len(expression.Args),
			)
			return false
		} else if len(expression.Fun.Args) > len(expression.Args) {
			printErrorln(
				expression.Loc(), "to few arguments to function, expected",
				len(expression.Fun.Args), "got", len(expression.Args),
			)
			return false
		}

		ok := true

		for i, gotArg := range expression.Args {
			expectedArg := expression.Fun.Args[i]

			ok = typeCheckExpression(gotArg, expectedArg.Typ) && ok

			ok = checkTypes(gotArg.Loc(), expectedArg.Typ, gotArg.ReturnType()) && ok
		}

		return ok

	case ast.Cast:
		if !typeCheckExpression(expression.Inner, nil) {
			return false
		}

		return checkCast(expression.Loc(), expression.Inner.ReturnType(), expression.NewType)

	case ast.StructIndex:
		return true

	case ast.ArrayIndex:
		return true

	case ast.Allocation:
		return true

	case ast.BinaryOpNode:
		return typeCheckBinOp(expression, expected)

	case ast.UnaryOpNode:
		return true

	default:
		panic("unreachable")
	}
}

func typeCheckBinOp(binOp ast.BinaryOpNode, expected ast.Type) bool {
	if ast.BOCount != 28 {
		panic("binarry opperation enum length changed")
	}

	lTyp, rTyp := binOp.Lhs.ReturnType(), binOp.Rhs.ReturnType()
	lInf, rInf := lTyp.Kind() == ast.KindInferred, rTyp.Kind() == ast.KindInferred
	ok := true

	if lInf {
		if rInf {
			lDefaultTyp := lTyp.(ast.InferredType).Default
			ok = typeCheckExpression(binOp.Lhs, lDefaultTyp) && ok
			ok = typeCheckExpression(binOp.Rhs, lDefaultTyp) && ok
		} else {
			ok = typeCheckExpression(binOp.Lhs, rTyp) && ok
			ok = typeCheckExpression(binOp.Rhs, rTyp) && ok
		}
	} else {
		if rInf {
			ok = typeCheckExpression(binOp.Lhs, lTyp) && ok
			ok = typeCheckExpression(binOp.Rhs, lTyp) && ok
		} else {
			ok = typeCheckExpression(binOp.Lhs, lTyp) && ok
			ok = typeCheckExpression(binOp.Rhs, rTyp) && ok
		}
	}

	if !ok {
		return false
	}

	lTyp, rTyp = binOp.Lhs.ReturnType(), binOp.Rhs.ReturnType()

	if !ast.EqualTypes(lTyp, rTyp) {
		printErrorln(
			binOp.Loc(), "mismatched types",
			typeToString(lTyp), "and", typeToString(rTyp),
		)
		return false
	}

	switch binOp.Op {
	case ast.BOEquals, ast.BONotEqual:
		fallthrough //arrays and pointers? not yet anyways

	// number and number
	case ast.BOPlus, ast.BOMinus, ast.BOMul,
		ast.BODiv, ast.BOPlusAssign, ast.BODashAssign,
		ast.BOStarAssign, ast.BOSlashAssign,
		ast.BOGt, ast.BOLt, ast.BOGtEq, ast.BOLtEq:

		if lTyp.Kind()&ast.KindNumberMask == 0 {
			printErrorln(
				binOp.Loc(), "invalid opperation",
				binOp.Op.String(), "on", typeToString(lTyp),
			)
			return false
		}

		return true

	// (u)int and (u)int
	case ast.BOMod, ast.BOBinAnd, ast.BOBinOr, ast.BOBinXor, ast.BOShl, ast.BOShr, ast.BOAndAssign, ast.BOOrAssign, ast.BOXorAssign, ast.BOShrAssign, ast.BOShlAssign:
		if lTyp.Kind()&(ast.KindInt|ast.KindUint) == 0 {
			printErrorln(
				binOp.Loc(), "invalid opperation",
				binOp.Op.String(), "on", typeToString(lTyp),
			)
			return false
		}

		return true

	// bool and bool
	case ast.BOBoolAnd, ast.BOBoolOr:
		if lTyp.Kind() != ast.KindBool {
			printErrorln(
				binOp.Loc(), "invalid opperation",
				binOp.Op.String(), "on", typeToString(lTyp),
			)
			return false
		}

		return true

	// any types
	case ast.BOAssign:
		return true

	case ast.BOCount:
		fallthrough

	default:
		panic("unknow binop")
	}
}

func checkCast(loc lexer.Location, inner, outer ast.Type) bool {

	eqalKinds := inner.Kind() == outer.Kind()
	intToUint := (inner.Kind() == ast.KindUint && outer.Kind() == ast.KindInt) ||
		(inner.Kind() == ast.KindInt && outer.Kind() == ast.KindUint)
	inferred := inner.Kind() == ast.KindInferred

	if !eqalKinds && !intToUint && !inferred {
		printErrorln(
			loc, "cannot cast expression of type",
			typeToString(inner), "to", typeToString(outer),
		)
		return false
	}

	switch inner.Kind() {
	case ast.KindInt:
		return true
	case ast.KindUint:
		return true
	case ast.KindFloat:
		return true
	case ast.KindBool:
		return true
	case ast.KindPointer:
		return true // maybe not allow pointer casts?
	case ast.KindArray:
		return ast.EqualTypes(inner.(ast.ArrayType).Inner, outer.(ast.ArrayType).Inner)

	case ast.KindStruct:
		inner, outer := inner.(ast.StructType), outer.(ast.StructType)
		inner.Name_ = ""
		outer.Name_ = ""

		if ast.EqualTypes(inner, outer) {
			return true
		}

		printErrorln(
			loc, "cannot cast expression of type",
			typeToString(inner), "to", typeToString(outer),
		)

		return false
	case ast.KindInterface:
		panic("not implemented")
	case ast.KindInferred:
		return checkCast(loc, inner.(ast.InferredType).Default, outer)
	default:
		panic("unreachable")
	}
}

func checkTypes(loc lexer.Location, expected, got ast.Type) bool {
	if !ast.EqualTypes(expected, got) && expected.Kind() != ast.KindAny {
		printErrorln(
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
	if typ.Name() != "" {
		sb.WriteString(string(typ.Name()))
		return
	}

	switch cur := typ.(type) {
	case ast.SimpleType:
		panic("unreachable, simpletype should always have a name")

	case ast.PointerType:
		sb.WriteByte('&')
		typeToStringSb(cur.Inner, sb)

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
		typeToStringSb(cur.Inner, sb)

	default:
		panic("unreachable")
	}

}

func printErrorln(loc lexer.Location, message ...any) {
	fmt.Fprint(os.Stderr, loc.String()+"ERROR: ")
	fmt.Fprintln(os.Stderr, message...)
}
