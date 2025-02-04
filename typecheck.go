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
		if !typeCheckExpression(expression.Inner, expression.NewType) {
			return false
		}

		return checkCast(expression.Loc(), expression.Inner.ReturnType(), expression.NewType)

	case *ast.StructIndex:
		if !typeCheckExpression(expression.Base, nil) {
			return false
		}

		if expression.Base.ReturnType().Kind() != ast.KindStruct {
			printErrorln(expression.FieldLoc, "trying to index expression which is not a struct")
			return false
		}

		baseTyp := expression.Base.ReturnType().(ast.StructType)

		field := baseTyp.GetField(expression.FieldName)

		if field == nil {
			printErrorln(expression.FieldLoc, "unknown field `", expression.FieldName, "`")
			return false
		}

		expression.Type = field.Typ

		return true

	case *ast.ArrayIndex:
		if !typeCheckExpression(expression.Array, nil) {
			return false
		}

		innerTyp := expression.Array.ReturnType()
		if innerTyp.Kind() != ast.KindArray {
			printErrorln(expression.Loc(), "indexing non-array")
			return false
		}

		if !typeCheckExpression(expression.Index, nil) {
			return false
		}

		if (expression.Index.ReturnType().Kind() != ast.KindInt &&
			expression.Index.ReturnType().Kind() != ast.KindUint) ||
			expression.Index.ReturnType().Size() != 8 {
			printErrorln(expression.Index.Loc(), "expected 64 bit int or uint type to index array")
			return false
		}

		innerTypArray := innerTyp.(ast.ArrayType)

		expression.Typ = innerTypArray.Inner

		return true

	case ast.Allocation:
		return true

	case ast.BinaryOpNode:
		return typeCheckBinOp(expression)

	case ast.UnaryOpNode:
		return typeCheckUnOp(expression)

	default:
		panic("unreachable")
	}
}

func typeCheckBinOp(binOp ast.BinaryOpNode) bool {
	if ast.BOCount != 28 {
		panic("binarry opperation enum length changed")
	}

	lTyp, rTyp := binOp.Lhs.ReturnType(), binOp.Rhs.ReturnType()
	ok := true

	if lTyp != nil && rTyp != nil && lTyp.Kind() == ast.KindInferred && rTyp.Kind() == ast.KindInferred {
		ok = typeCheckExpression(binOp.Lhs, nil) && ok
		lTyp = binOp.Lhs.ReturnType()
		ok = typeCheckExpression(binOp.Rhs, lTyp) && ok
	} else {
		ok = typeCheckExpression(binOp.Lhs, rTyp) && ok
		ok = typeCheckExpression(binOp.Rhs, lTyp) && ok
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

	var kindMask ast.Kind
	switch binOp.Op {
	case ast.BOEquals, ast.BONotEqual:
		fallthrough // TODO: equality check for pointers, structs, strings,...

	// number and number
	case ast.BOPlus, ast.BOMinus, ast.BOMul,
		ast.BODiv, ast.BOPlusAssign, ast.BODashAssign,
		ast.BOStarAssign, ast.BOSlashAssign,
		ast.BOGt, ast.BOLt, ast.BOGtEq, ast.BOLtEq:

		kindMask = ast.KindNumberMask

	// (u)int and (u)int
	case ast.BOMod, ast.BOBinAnd, ast.BOBinOr, ast.BOBinXor, ast.BOShl, ast.BOShr, ast.BOAndAssign, ast.BOOrAssign, ast.BOXorAssign, ast.BOShrAssign, ast.BOShlAssign:
		kindMask = ast.KindInt | ast.KindUint

	// bool and bool
	case ast.BOBoolAnd, ast.BOBoolOr:
		kindMask = ast.KindBool

	// any types
	case ast.BOAssign:
		kindMask = ast.KindAny

	case ast.BOCount:
		fallthrough

	default:
		panic("unknow binop")
	}

	if lTyp.Kind()&kindMask == 0 {
		printErrorln(
			binOp.Loc(), "invalid opperation",
			binOp.Op.String(), "on", typeToString(lTyp),
		)
		return false
	}

	switch binOp.Op {
	case ast.BOAssign, ast.BOPlusAssign, ast.BODashAssign, ast.BOStarAssign,
		ast.BOSlashAssign, ast.BOAndAssign, ast.BOOrAssign, ast.BOXorAssign,
		ast.BOShrAssign, ast.BOShlAssign:

		if !canAssign(binOp.Lhs) {
			printErrorln(
				binOp.Loc(), "cannot assign",
			)
			return false
		}
	}

	return true
}

func typeCheckUnOp(unOp ast.UnaryOpNode) bool {
	if ast.UOCount != 6 {
		panic("unary opperation enum length changed")
	}

	typeCheckExpression(unOp.Expression, nil)

	var kindMask ast.Kind
	switch unOp.Op {
	case ast.UOPlus, ast.UONegative:
		kindMask = ast.KindNumberMask

	case ast.UOBoolNot:
		kindMask = ast.KindBool

	case ast.UOBinNot:
		kindMask = ast.KindInt | ast.KindUint

	case ast.UORef:
		if !canRef(unOp.Expression) {
			printErrorln(unOp.Loc(), "cannot take reference to value")
			return false
		}

		kindMask = ast.KindAny

	case ast.UODeref:
		kindMask = ast.KindPointer
	}

	if unOp.Expression.ReturnType().Kind()&kindMask == 0 {
		printErrorln(
			unOp.Loc(), "invalid opperation",
			unOp.Op.String(), "on", typeToString(unOp.Expression.ReturnType()),
		)
		return false
	}

	return true
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

func canAssign(expr ast.Expression) bool {
	switch expr := expr.(type) {
	case ast.Var:
		return !expr.IsConst

	case ast.UnaryOpNode:
		if expr.Op == ast.UODeref {
			return canAssign(expr.Expression)
		}

		return false

	case *ast.StructIndex:
		return canAssign(expr.Base)

	case *ast.ArrayIndex:
		return canAssign(expr.Array)

	default:
		return false
	}
}

func canRef(expr ast.Expression) bool {
	switch expr := expr.(type) {
	case ast.Var:
		return true

	case ast.UnaryOpNode:
		if expr.Op == ast.UODeref {
			return canRef(expr.Expression)
		}

		return false

	case *ast.StructIndex:
		return canRef(expr.Base)

	case *ast.ArrayIndex:
		return false

	default:
		return false
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
