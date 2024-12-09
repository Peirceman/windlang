package main

import (
	"encoding/binary"
	"errors"
	"io"
	"strconv"
)

// TODO: shadowing wont work
type BytecodeGenerator struct {
	Output io.WriteSeeker
	data map[uint32][]byte
	instructionIdx int
	bytesWritten int
	vars map[Identifier]uint32
	nextRuntimeVarIdx uint32
	nextCompiletimeVarIdx uint32
	
}

func (g *BytecodeGenerator) GenerateBytecode(code CodeBlockNode) error {
	_, exists := code.scope.Funcs["main"]
	if !exists {
		return errors.New("Error generating bytecode: no main function found")
	}

	g.data = make(map[uint32][]byte)
	g.vars = make(map[Identifier]uint32)
	g.bytesWritten = 0
	g.instructionIdx = 0
	g.nextRuntimeVarIdx = 1
	g.nextCompiletimeVarIdx = 0x80000001

	_, err := g.Output.Write([]byte{'W', 'B', 'C', 0, 0, 0, 0, 0})

	if err != nil {
		return err
	}

	g.Output.Write([]byte("code"))
	codeSectionLengthSeek, err := g.Output.Seek(4, io.SeekCurrent)

	if err != nil {
		return err
	}

	codeSectionLengthSeek -= 4

	err = g.writeCodeBlock(code)

	if err != nil {
		return err
	}

	_, err = g.Output.Seek(codeSectionLengthSeek, io.SeekStart)

	if err != nil {
		return err
	}

	_, err = g.Output.Write([]byte{
		byte(g.bytesWritten >> (3 * 8)),
		byte(g.bytesWritten >> (2 * 8)),
		byte(g.bytesWritten >> (1 * 8)),
		byte(g.bytesWritten >> (0 * 8)),
	})

	return err
}

func (g *BytecodeGenerator) writeCodeBlock(codeBlock CodeBlockNode) error {
	for _, node := range codeBlock.Statements {
		switch node := node.(type) {
		case ExpressionNode:
			err := g.writeExpression(node.Expr)

			if err != nil {
				return err
			}

		case ConstNode:
			_, err := g.Output.Write([]byte{byte(decl), byte(node.typ.kind & KindSizeMask)})

			if err != nil {
				return err
			}

			err = binary.Write(g.Output, binary.BigEndian, g.nextRuntimeVarIdx)

			if err != nil {
				return err
			}

			g.bytesWritten += 6
			g.instructionIdx++
			g.vars[node.name] = g.nextRuntimeVarIdx
			g.nextRuntimeVarIdx++

			err = g.writeExpression(node.Value)

			if err != nil {
				return err
			}

			g.Output.Write([]byte{byte(popv), byte(node.typ.kind & KindSizeMask)})
			binary.Write(g.Output, binary.BigEndian, g.nextRuntimeVarIdx - 1)
			g.bytesWritten += 6
			g.instructionIdx++
		case VarNode:
			_, err := g.Output.Write([]byte{byte(decl), byte(node.typ.kind & KindSizeMask)})

			if err != nil {
				return err
			}

			err = binary.Write(g.Output, binary.BigEndian, g.nextRuntimeVarIdx)

			if err != nil {
				return err
			}

			g.bytesWritten += 6
			g.instructionIdx++
			g.vars[node.name] = g.nextRuntimeVarIdx
			g.nextRuntimeVarIdx++

			err = g.writeExpression(node.Value)

			if err != nil {
				return err
			}

			g.Output.Write([]byte{byte(popv), byte(node.typ.kind & KindSizeMask)})
			binary.Write(g.Output, binary.BigEndian, g.nextRuntimeVarIdx - 1)
			g.bytesWritten += 6
			g.instructionIdx++

		case FuncNode:
			if !(node.name == "main" && len(node.Args) == 0) {
				panic("Error, functions other than main have not been implemented yet")
			}

			err := g.writeCodeBlock(node.Body)

			if err != nil {
				return err
			}

		case CodeBlockNode:
			startingRuntimeIdx := g.nextRuntimeVarIdx

			err := g.writeCodeBlock(node)

			if err != nil {
				return err
			}

			for k, _ := range g.data {
				if ((k & 0x80000000) == 0) && k >= startingRuntimeIdx {
					delete(g.data, k)
				}
			}

			g.nextRuntimeVarIdx = startingRuntimeIdx

		case IfChain:
			panic("TODO: implement ifchains")
		default:
			panic("Unimplemented type: " + node.String())
		}
	}

	return nil
}

func (g *BytecodeGenerator) writeExpression(expression Expression) error {

	switch expression := expression.(type) {
	case IntLit:
		size := expression.returnType().kind & KindSizeMask
		_, err := g.Output.Write([]byte{byte(push), byte(size)})

		if err != nil {
			return err
		}

		switch size {
		case Kind8:
			err = binary.Write(g.Output, binary.BigEndian, uint8(expression.value))
		case Kind16:
			err = binary.Write(g.Output, binary.BigEndian, uint16(expression.value))
		case Kind32:
			err = binary.Write(g.Output, binary.BigEndian, uint32(expression.value))
		case Kind64:
			err = binary.Write(g.Output, binary.BigEndian, uint64(expression.value))
		}

		if err != nil {
			return err
		}

		g.bytesWritten += 2 + int(size)
		g.instructionIdx++

	case FloatLit:
		size := expression.returnType().kind & KindSizeMask
		float, _ := strconv.ParseFloat(expression.value, int(size*8)) // at this point it should be a valid float
		g.bytesWritten = 2 + int(size)
		g.instructionIdx++

		_, err := g.Output.Write([]byte{byte(push), byte(size)})

		if err != nil {
			return err
		}

		switch size {
		case Kind32:
			err = binary.Write(g.Output, binary.BigEndian, float32(float))
		case Kind64:
			err = binary.Write(g.Output, binary.BigEndian, float)
		}

		if err != nil {
			return err
		}

	case StrLit:
		g.data[g.nextCompiletimeVarIdx] = []byte(expression.value)
		g.nextCompiletimeVarIdx++
		return nil // cant realy push a pointer yet but this should only be used by prts? so handle on that level?


	case CharLit:
		_, err := g.Output.Write([]byte{byte(push), 4})

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		err = binary.Write(g.Output, binary.BigEndian, expression.value)

		if err != nil {
			return err
		}


	case BoolLit:
		if expression.value {
			_, err := g.Output.Write([]byte{byte(push), 4, 0, 0, 0, 1})

			if err != nil {
				return err
			}
		} else {
			_, err := g.Output.Write([]byte{byte(push), 4, 0, 0, 0, 0})

			if err != nil {
				return err
			}
		}

	case Var:
		_, err := g.Output.Write([]byte{byte(pshv), byte(expression.typ.kind & KindSizeMask)})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[expression.name])
		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case Const:
		_, err := g.Output.Write([]byte{byte(pshv), byte(expression.typ.kind & KindSizeMask)})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[expression.name])

		g.bytesWritten += 6
		g.instructionIdx++

	case Func:
		// I beleive this is a function object?, definitely not implemented

	case FuncCall:
		panic("functions not implemented yet")

	case BinaryOpNode:
		return g.generateBinaryOpNode(expression)
	}

	return nil
}

func (g *BytecodeGenerator) generateBinaryOpNode(binopnode BinaryOpNode) error {
	switch binopnode.Op {
	case BOPlus:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		kind := binopnode.Lhs.returnType().kind
		if kind & KindInt != 0 {
			_, err = g.Output.Write([]byte{byte(adds), byte(kind & KindSizeMask)})
		} else {
			_, err = g.Output.Write([]byte{byte(addf), byte(kind & KindSizeMask)})
		}

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

	case BOMinus:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		kind := binopnode.Lhs.returnType().kind
		if kind & KindInt != 0 {
			_, err = g.Output.Write([]byte{byte(subs), byte(kind & KindSizeMask)})
		} else {
			_, err = g.Output.Write([]byte{byte(subf), byte(kind & KindSizeMask)})
		}

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

	case BOMul:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		kind := binopnode.Lhs.returnType().kind
		if kind & KindInt != 0 {
			_, err = g.Output.Write([]byte{byte(muls), byte(kind & KindSizeMask)})
		} else {
			_, err = g.Output.Write([]byte{byte(mulf), byte(kind & KindSizeMask)})
		}

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

	case BODiv:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		kind := binopnode.Lhs.returnType().kind
		if kind & KindInt != 0 {
			_, err = g.Output.Write([]byte{byte(divs), byte(kind & KindSizeMask)})
		} else {
			_, err = g.Output.Write([]byte{byte(divf), byte(kind & KindSizeMask)})
		}

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

	case BOBinAnd:
		panic("unimplemented")
	case BOBinOr:
		panic("unimplemented")
	case BOBinXor:
		panic("unimplemented")
	case BOBoolAnd:
		panic("unimplemented")
	case BOBoolOr:
		panic("unimplemented")

	case BOShl:
		panic("unimplemented")
	case BOShr:
		panic("unimplemented")
	case BOGt:
		panic("unimplemented")
	case BOLt:
		panic("unimplemented")
	case BOGtEq:
		panic("unimplemented")
	case BOLtEq:
		panic("unimplemented")
	case BOEquals:
		panic("unimplemented")
	case BONotEqual:
		panic("unimplemented")

	case BOAssign:
		lhs, ok := binopnode.Lhs.(Var)
		if !ok {
			panic("assigning to not variable???")
		}

		g.writeExpression(binopnode.Rhs)


		idx := g.vars[lhs.name]
		size := lhs.returnType().kind & KindSizeMask

		_, err := g.Output.Write([]byte{byte(popv), byte(size)})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, idx)

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case BOPlusAssign:
		panic("unimplemented")
	case BODashAssign:
		panic("unimplemented")
	case BOStarAssign:
		panic("unimplemented")
	case BOSlashAssign:
		panic("unimplemented")
	case BoAndAssign:
		panic("unimplemented")
	case BoOrAssign:
		panic("unimplemented")
	case BoXorAssign:
		panic("unimplemented")
	case BOShrAssign:
		panic("unimplemented")
	case BOShlAssign:
		panic("unimplemented")

	default:
		panic("unreachable")
	}

	return nil
}