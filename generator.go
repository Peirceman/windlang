package main

import (
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"strconv"
)

// TODO: shadowing wont work
type BytecodeGenerator struct {
	Output                io.WriteSeeker
	data                  map[uint32][]byte
	instructionIdx        int
	bytesWritten          int
	vars                  map[Identifier]uint32
	nextRuntimeVarIdx     uint32
	nextCompiletimeVarIdx uint32
}

func GenerateBytecode(Output io.WriteSeeker, code CodeBlockNode) error {
	g := &BytecodeGenerator{Output: Output}

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
	seek, err := g.Output.Seek(4, io.SeekCurrent)

	if err != nil {
		return err
	}

	seek -= 4

	err = g.writeCodeBlock(code)

	if err != nil {
		return err
	}

	_, err = g.Output.Seek(seek, io.SeekStart)

	if err != nil {
		return err
	}

	_, err = g.Output.Write([]byte{
		byte(g.bytesWritten >> (3 * 8)),
		byte(g.bytesWritten >> (2 * 8)),
		byte(g.bytesWritten >> (1 * 8)),
		byte(g.bytesWritten >> (0 * 8)),
	})

	if err != nil {
		return err
	}

	_, err = g.Output.Seek(0, io.SeekEnd)

	if err != nil {
		return err
	}

	_, err = g.Output.Write([]byte("data"))

	if err != nil {
		return err
	}

	seek, err = g.Output.Seek(4, io.SeekCurrent)

	if err != nil {
		return err
	}

	seek -= 4

	g.bytesWritten = 0

	for id, data := range g.data {
		err = binary.Write(g.Output, binary.BigEndian, id)

		if err != nil {
			return err
		}

		g.bytesWritten += 4

		err = binary.Write(g.Output, binary.BigEndian, uint32(len(data)))

		if err != nil {
			return err
		}

		g.bytesWritten += 4

		_, err = g.Output.Write(data)

		if err != nil {
			return err
		}

		g.bytesWritten += len(data)
	}

	_, err = g.Output.Seek(seek, io.SeekStart)

	if err != nil {
		return err
	}

	_, err = g.Output.Write([]byte{
		byte(g.bytesWritten >> (3 * 8)),
		byte(g.bytesWritten >> (2 * 8)),
		byte(g.bytesWritten >> (1 * 8)),
		byte(g.bytesWritten >> (0 * 8)),
	})

	if err != nil {
		return err
	}

	return nil
}

func (g *BytecodeGenerator) writeCodeBlock(codeBlock CodeBlockNode) error {
	startingRuntimeVarIndex := g.nextRuntimeVarIdx

	for identifier, varDef := range codeBlock.scope.vars {
		_, exists := g.vars[identifier]
		if exists {
			panic("shadowing not implemented yet")
		}

		if varDef.returnType().kind&KindString&KindTypeMask != 0 {
			continue
		}

		g.vars[identifier] = g.nextRuntimeVarIdx
		g.nextRuntimeVarIdx++
	}

	for identifier, constDef := range codeBlock.scope.consts {
		_, exists := g.vars[identifier]
		if exists {
			panic("shadowing not implemented yet")
		}

		if constDef.returnType().kind&KindString&KindTypeMask != 0 {
			continue
		}

		g.vars[identifier] = g.nextRuntimeVarIdx
		g.nextRuntimeVarIdx++
	}

	for _, node := range codeBlock.Statements {
		switch node := node.(type) {
		case ExpressionNode:
			err := g.writeExpression(node.Expr)

			if err != nil {
				return err
			}

		case ConstNode:
			// very stupid
			if (node.typ.kind & (KindString & KindTypeMask)) != 0 {
				err := g.writeExpression(node.Value)

				if err != nil {
					return err
				}

				g.vars[node.name] = g.nextCompiletimeVarIdx - 1
				break
			}

			_, err := g.Output.Write([]byte{byte(decl), byte(node.typ.kind & KindSizeMask)})

			if err != nil {
				return err
			}

			err = binary.Write(g.Output, binary.BigEndian, g.vars[node.name])

			if err != nil {
				return err
			}

			g.bytesWritten += 6
			g.instructionIdx++

			if node.Value == nil {
				break
			}

			err = g.writeExpression(node.Value)

			if err != nil {
				return err
			}

			g.Output.Write([]byte{byte(popv), byte(node.typ.kind & KindSizeMask)})
			binary.Write(g.Output, binary.BigEndian, g.vars[node.name])
			g.bytesWritten += 6
			g.instructionIdx++

		case VarNode:
			if (node.typ.kind & (KindString & KindTypeMask)) != 0 {
				err := g.writeExpression(node.Value)

				if err != nil {
					return err
				}

				g.vars[node.name] = g.nextCompiletimeVarIdx - 1
				break
			}

			_, err := g.Output.Write([]byte{byte(decl), byte(node.typ.kind & KindSizeMask)})

			if err != nil {
				return err
			}

			err = binary.Write(g.Output, binary.BigEndian, g.vars[node.name])

			if err != nil {
				return err
			}

			g.bytesWritten += 6
			g.instructionIdx++

			if node.Value == nil {
				break
			}

			err = g.writeExpression(node.Value)

			if err != nil {
				return err
			}

			g.Output.Write([]byte{byte(popv), byte(node.typ.kind & KindSizeMask)})
			binary.Write(g.Output, binary.BigEndian, g.vars[node.name])
			g.bytesWritten += 6
			g.instructionIdx++

		case FuncNode:
			if node.name != "main" && len(node.Args) == 0 {
				panic("Error, functions other than main have not been implemented yet")
			}

			err := g.writeCodeBlock(node.Body)

			if err != nil {
				return err
			}

		case CodeBlockNode:
			err := g.writeCodeBlock(node)

			if err != nil {
				return err
			}

		case IfChain:
			err := g.writeIfChain(node)

			if err != nil {
				return err
			}

		default:
			panic("Unimplemented type: " + node.String())
		}
	}

	g.nextRuntimeVarIdx = startingRuntimeVarIndex
	return nil
}

func (g *BytecodeGenerator) writeIfChain(chain IfChain) error {
	var seekEnds []int64
	var seekFalse int64

	err := g.writeExpression(chain.IfCondition)

	if err != nil {
		return err
	}

	_, err = g.Output.Write([]byte{byte(jpfl), byte(chain.IfCondition.returnType().kind & KindSizeMask)})

	if err != nil {
		return err
	}

	seekFalse, err = g.Output.Seek(4, io.SeekCurrent)

	if err != nil {
		return err
	}

	seekFalse -= 4
	g.bytesWritten += 6
	g.instructionIdx++

	err = g.writeCodeBlock(chain.IfStatement)

	if err != nil {
		return err
	}

	if chain.hasElse || len(chain.ElifConditions) > 0 {
		_, err = g.Output.Write([]byte{byte(jump), 0})

		if err != nil {
			return err
		}

		seekEnd, err := g.Output.Seek(4, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekEnd -= 4
		g.bytesWritten += 6
		g.instructionIdx++
		seekEnds = append(seekEnds, seekEnd)
	}

	tmp, _ := g.Output.Seek(0, io.SeekCurrent)

	_, err = g.Output.Seek(seekFalse, io.SeekStart)

	if err != nil {
		return err
	}

	err = binary.Write(g.Output, binary.BigEndian, uint32(g.instructionIdx))

	if err != nil {
		return err
	}

	_, err = g.Output.Seek(tmp, io.SeekStart)

	if err != nil {
		return err
	}

	for i, condition := range chain.ElifConditions {
		statement := chain.ElifStatements[i]

		err := g.writeExpression(condition)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(jpfl), byte(condition.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		seekFalse, err = g.Output.Seek(4, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekFalse -= 4
		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeCodeBlock(statement)

		if err != nil {
			return err
		}

		if chain.hasElse || i < len(chain.ElifConditions)-1 {
			_, err = g.Output.Write([]byte{byte(jump), 0})

			if err != nil {
				return err
			}

			seekEnd, err := g.Output.Seek(4, io.SeekCurrent)

			if err != nil {
				return err
			}

			seekEnd -= 4
			g.bytesWritten += 6
			g.instructionIdx++
			seekEnds = append(seekEnds, seekEnd)
		}

		tmp, _ := g.Output.Seek(0, io.SeekCurrent)

		_, err = g.Output.Seek(seekFalse, io.SeekStart)

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, uint32(g.instructionIdx))

		if err != nil {
			return err
		}

		_, err = g.Output.Seek(tmp, io.SeekStart)

		if err != nil {
			return err
		}
	}

	if chain.hasElse {
		err = g.writeCodeBlock(chain.ElseStatement)

		if err != nil {
			return err
		}
	}

	for _, seek := range seekEnds {
		_, err = g.Output.Seek(seek, io.SeekStart)

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, uint32(g.instructionIdx))

		if err != nil {
			return err
		}
	}

	_, err = g.Output.Seek(0, io.SeekEnd)

	if err != nil {
		return err
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

		g.bytesWritten += 6
		g.instructionIdx++

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
		if expression.fun.name == "print" || expression.fun.name == "println" {
			if len(expression.Args) != 1 {
				panic(fmt.Errorf("Exactly one argument expected for %s", expression.fun.name))
			}

			arg := expression.Args[0]
			switch arg.returnType().kind & KindTypeMask {
			case KindInt, KindBool & KindTypeMask:
				err := g.writeExpression(arg)

				if err != nil {
					return err
				}

				_, err = g.Output.Write([]byte{byte(prtu), byte(arg.returnType().kind & KindSizeMask)})

				if err != nil {
					return err
				}

				g.bytesWritten += 2
				g.instructionIdx++

			case KindFloat:
				err := g.writeExpression(arg)

				if err != nil {
					return err
				}

				_, err = g.Output.Write([]byte{byte(prtf), byte(arg.returnType().kind & KindSizeMask)})

				if err != nil {
					return err
				}

				g.bytesWritten += 2
				g.instructionIdx++

			case KindString & KindTypeMask:
				_, err := g.Output.Write([]byte{byte(prts), byte(arg.returnType().kind & KindSizeMask)})

				if err != nil {
					return err
				}

				if aVar, ok := arg.(Var); ok {
					name := aVar.name
					err = binary.Write(g.Output, binary.BigEndian, g.vars[name])
				} else if aConst, ok := arg.(Const); ok {
					name := aConst.name
					err = binary.Write(g.Output, binary.BigEndian, g.vars[name])
				} else if aLit, ok := arg.(StrLit); ok {
					err = g.writeExpression(aLit)

					if err != nil {
						return err
					}

					err = binary.Write(g.Output, binary.BigEndian, g.nextCompiletimeVarIdx-1)
				}

				g.bytesWritten += 6
				g.instructionIdx++

			}

			break
		}

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
		if kind&KindInt != 0 {
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
		if kind&KindInt != 0 {
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
		if kind&KindInt != 0 {
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
		if kind&KindInt != 0 {
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
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		size := byte(binopnode.returnType().kind & KindSizeMask)

		_, err = g.Output.Write([]byte{byte(band), size})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

	case BOBinOr:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		size := byte(binopnode.returnType().kind & KindSizeMask)

		_, err = g.Output.Write([]byte{byte(borr), size})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

	case BOBinXor:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		size := byte(binopnode.returnType().kind & KindSizeMask)

		_, err = g.Output.Write([]byte{byte(bxor), size})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

	case BOBoolAnd:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(jpfl), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		seekFalse, err := g.Output.Seek(4, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekFalse -= 4
		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(jptr), byte(binopnode.Rhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		seekTrue, err := g.Output.Seek(4, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekTrue -= 4
		g.bytesWritten += 6
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(push), 4, 0, 0, 0, 0})

		if err != nil {
			return err
		}

		g.instructionIdx++
		g.bytesWritten += 6

		_, err = g.Output.Seek(seekFalse, io.SeekStart)

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, uint32(g.instructionIdx-1))

		if err != nil {
			return err
		}

		_, err = g.Output.Seek(0, io.SeekEnd)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(jump), 0})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, uint32(g.instructionIdx+2))

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		_, err = g.Output.Seek(seekTrue, io.SeekStart)

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, uint32(g.instructionIdx))

		if err != nil {
			return err
		}

		_, err = g.Output.Seek(0, io.SeekEnd)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(push), 4, 0, 0, 0, 1})

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case BOBoolOr:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(jptr), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		seekTrue, err := g.Output.Seek(4, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekTrue -= 4
		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(jpfl), byte(binopnode.Rhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		seekFalse, err := g.Output.Seek(4, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekFalse -= 4
		g.bytesWritten += 6
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(push), 4, 0, 0, 0, 1})

		if err != nil {
			return err
		}

		g.instructionIdx++
		g.bytesWritten += 6

		_, err = g.Output.Seek(seekTrue, io.SeekStart)

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, uint32(g.instructionIdx-1))

		if err != nil {
			return err
		}

		_, err = g.Output.Seek(0, io.SeekEnd)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(jump), 0})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, uint32(g.instructionIdx+2))

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		_, err = g.Output.Seek(seekFalse, io.SeekStart)

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, uint32(g.instructionIdx))

		if err != nil {
			return err
		}

		_, err = g.Output.Seek(0, io.SeekEnd)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(push), 4, 0, 0, 0, 0})

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case BOShl:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		size := byte(binopnode.returnType().kind & KindSizeMask)

		_, err = g.Output.Write([]byte{byte(bshl), size})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

	case BOShr:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		size := byte(binopnode.returnType().kind & KindSizeMask)

		_, err = g.Output.Write([]byte{byte(bsrs), size})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

	case BOGt:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(cmps), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(isgt), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		requiredSize := byte(binopnode.returnType().kind & KindSizeMask)
		curSize := byte(binopnode.Lhs.returnType().kind & KindSizeMask)

		g.castUnsigned(requiredSize, curSize)

	case BOLt:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(cmps), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(islt), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		requiredSize := byte(binopnode.returnType().kind & KindSizeMask)
		curSize := byte(binopnode.Lhs.returnType().kind & KindSizeMask)

		g.castUnsigned(requiredSize, curSize)
	case BOGtEq:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(cmps), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(isge), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		requiredSize := byte(binopnode.returnType().kind & KindSizeMask)
		curSize := byte(binopnode.Lhs.returnType().kind & KindSizeMask)

		g.castUnsigned(requiredSize, curSize)

	case BOLtEq:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(cmps), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(isle), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		requiredSize := byte(binopnode.returnType().kind & KindSizeMask)
		curSize := byte(binopnode.Lhs.returnType().kind & KindSizeMask)

		g.castUnsigned(requiredSize, curSize)
	case BOEquals:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(cmps), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(iseq), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		requiredSize := byte(binopnode.returnType().kind & KindSizeMask)
		curSize := byte(binopnode.Lhs.returnType().kind & KindSizeMask)

		g.castUnsigned(requiredSize, curSize)

	case BONotEqual:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(cmps), byte(binopnode.Lhs.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(isne), byte(binopnode.returnType().kind & KindSizeMask)})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		requiredSize := byte(binopnode.returnType().kind & KindSizeMask)
		curSize := byte(binopnode.Lhs.returnType().kind & KindSizeMask)

		g.castUnsigned(requiredSize, curSize)

	case BOAssign:
		lhs, ok := binopnode.Lhs.(Var)
		if !ok {
			panic("assigning to not variable???")
		}

		g.writeExpression(binopnode.Rhs)

		idx := g.vars[lhs.name]

		// TODO: this is the hackiest shit ever, should be solved when pointers exist
		if (lhs.returnType().kind & KindString & KindTypeMask) != 0 {
			break
		}

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
		lhs, ok := binopnode.Lhs.(Var)
		size := byte(lhs.typ.kind & KindSizeMask)
		if !ok {
			panic("assigning to not var??")
		}

		_, err := g.Output.Write([]byte{byte(pshv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch lhs.typ.kind & KindTypeMask {
		case KindInt:
			_, err = g.Output.Write([]byte{byte(adds), size})
		case KindFloat:
			_, err = g.Output.Write([]byte{byte(addf), size})
		}

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(popv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case BODashAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := byte(lhs.typ.kind & KindSizeMask)
		if !ok {
			panic("assigning to not var??")
		}

		_, err := g.Output.Write([]byte{byte(pshv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch lhs.typ.kind & KindTypeMask {
		case KindInt:
			_, err = g.Output.Write([]byte{byte(subs), size})
		case KindFloat:
			_, err = g.Output.Write([]byte{byte(subf), size})
		}

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(popv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case BOStarAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := byte(lhs.typ.kind & KindSizeMask)
		if !ok {
			panic("assigning to not var??")
		}

		_, err := g.Output.Write([]byte{byte(pshv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch lhs.typ.kind & KindTypeMask {
		case KindInt:
			_, err = g.Output.Write([]byte{byte(muls), size})
		case KindFloat:
			_, err = g.Output.Write([]byte{byte(mulf), size})
		}

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(popv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case BOSlashAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := byte(lhs.typ.kind & KindSizeMask)
		if !ok {
			panic("assigning to not var??")
		}

		_, err := g.Output.Write([]byte{byte(pshv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch lhs.typ.kind & KindTypeMask {
		case KindInt:
			_, err = g.Output.Write([]byte{byte(divs), size})
		case KindFloat:
			_, err = g.Output.Write([]byte{byte(divf), size})
		}

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(popv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case BoAndAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := byte(lhs.typ.kind & KindSizeMask)
		if !ok {
			panic("assigning to not var??")
		}

		_, err := g.Output.Write([]byte{byte(pshv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(band), size})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(popv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case BoOrAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := byte(lhs.typ.kind & KindSizeMask)
		if !ok {
			panic("assigning to not var??")
		}

		_, err := g.Output.Write([]byte{byte(pshv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(borr), size})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(popv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case BoXorAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := byte(lhs.typ.kind & KindSizeMask)
		if !ok {
			panic("assigning to not var??")
		}

		_, err := g.Output.Write([]byte{byte(pshv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(bxor), size})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(popv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case BOShrAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := byte(lhs.typ.kind & KindSizeMask)
		if !ok {
			panic("assigning to not var??")
		}

		_, err := g.Output.Write([]byte{byte(pshv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(bsrs), size})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(popv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	case BOShlAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := byte(lhs.typ.kind & KindSizeMask)
		if !ok {
			panic("assigning to not var??")
		}

		_, err := g.Output.Write([]byte{byte(pshv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		_, err = g.Output.Write([]byte{byte(bshl), size})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(popv), size})

		if err != nil {
			return err
		}

		err = binary.Write(g.Output, binary.BigEndian, g.vars[lhs.name])

		if err != nil {
			return err
		}

		g.bytesWritten += 6
		g.instructionIdx++

	default:
		panic("unreachable")
	}

	return nil
}

func (g *BytecodeGenerator) castUnsigned(requiredSize, currentSize byte) (err error) {
	for currentSize > requiredSize {
		currentSize /= 2

		_, err = g.Output.Write([]byte{byte(swap), currentSize})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++

		_, err = g.Output.Write([]byte{byte(pops), currentSize})

		if err != nil {
			return err
		}

		g.bytesWritten += 2
		g.instructionIdx++
	}

	for currentSize < requiredSize {

		_, err = g.Output.Write([]byte{byte(push), currentSize})

		if err != nil {
			return err
		}

		_, err = g.Output.Write(make([]byte, currentSize))

		if err != nil {
			return err
		}

		g.instructionIdx++
		g.bytesWritten += 2 + int(currentSize)

		_, err = g.Output.Write([]byte{byte(swap), currentSize})

		if err != nil {
			return err
		}

		currentSize *= 2
	}

	return nil
}
