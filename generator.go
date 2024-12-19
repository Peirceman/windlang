package main

import (
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"math"
	"strconv"
)

// TODO: shadowing wont work
type BytecodeGenerator struct {
	Output                io.WriteSeeker
	data                  map[uint32][]byte
	instructionIdx        int
	bytesWritten          int
	vars                  map[Identifier]uint32
	funcs                 map[Identifier]uint32
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
	g.funcs = make(map[Identifier]uint32)
	g.bytesWritten = 0
	g.instructionIdx = 0
	g.nextRuntimeVarIdx = 1
	g.nextCompiletimeVarIdx = 0x80000001

	_, err := g.Output.Write([]byte{'W', 'B', 'C', 0, 0, 0, 0, 0})

	if err != nil {
		return err
	}

	_, err = g.Output.Write([]byte{'c', 'o', 'd', 'e', 0, 0, 0, 0})

	if err != nil {
		return err
	}

	seek, err := g.Output.Seek(0, io.SeekCurrent)

	if err != nil {
		return err
	}

	seek -= 4

	err = g.writeCodeBlock(code)

	if err != nil {
		return err
	}

	err = g.writeInstruction4(call, 0, g.funcs["main"])

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

	_, err = g.Output.Write([]byte{'d', 'a', 't', 'a', 0, 0, 0, 0})

	if err != nil {
		return err
	}

	seek, err = g.Output.Seek(0, io.SeekCurrent)

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

			err := g.writeInstruction4(decl, node.typ.kind&KindSizeMask, g.vars[node.name])

			if err != nil {
				return err
			}

			if node.Value == nil {
				break
			}

			err = g.writeExpression(node.Value)

			if err != nil {
				return err
			}

			err = g.writeInstruction4(popv, node.typ.kind&KindSizeMask, g.vars[node.name])

			if err != nil {
				return err
			}

		case VarNode:
			if (node.typ.kind & KindString & KindTypeMask) != 0 {
				err := g.writeExpression(node.Value)

				if err != nil {
					return err
				}

				g.vars[node.name] = g.nextCompiletimeVarIdx - 1
				break
			}

			err := g.writeInstruction4(decl, node.typ.kind&KindSizeMask, g.vars[node.name])

			if err != nil {
				return err
			}

			if node.Value == nil {
				break
			}

			err = g.writeExpression(node.Value)

			if err != nil {
				return err
			}

			err = g.writeInstruction4(popv, node.typ.kind&KindSizeMask, g.vars[node.name])

			if err != nil {
				return err
			}

		case FuncNode:
			err := g.writeInstruction4(jump, 0, 0)

			if err != nil {
				return err
			}

			seek, err := g.Output.Seek(0, io.SeekCurrent)

			if err != nil {
				return err
			}

			seek -= 4

			g.funcs[node.name] = uint32(g.instructionIdx)

			for _, arg := range node.Args {
				identifier := arg.name

				idx, exists := g.vars[identifier]
				if exists && idx < g.nextRuntimeVarIdx {
					panic("shadowing not implemented yet")
				}

				if arg.returnType().kind&KindString&KindTypeMask != 0 {
					panic("string arguments not implemented yet")
				}

				g.vars[identifier] = g.nextRuntimeVarIdx
				err = g.writeInstruction4(decl, arg.typ.kind&KindSizeMask, g.nextRuntimeVarIdx)

				if err != nil {
					return err
				}

				err = g.writeInstruction4(popv, arg.typ.kind&KindSizeMask, g.nextRuntimeVarIdx)

				if err != nil {
					return err
				}

				g.nextRuntimeVarIdx++

			}

			err = g.writeCodeBlock(node.Body)

			if err != nil {
				return err
			}

			_, err = g.Output.Seek(seek, io.SeekStart)

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

		case ReturnNode:
			if node.Expr != nil {
				err := g.writeExpression(node.Expr)

				if err != nil {
					return err
				}
			}

			err := g.writeInstruction0(rett, 0)

			if err != nil {
				return err
			}

		default:
			panic("Unimplemented type: " + node.String())
		}
	}

	return nil
}

func (g *BytecodeGenerator) writeIfChain(chain IfChain) error {
	var seekEnds []int64
	var seekFalse int64

	err := g.writeExpression(chain.IfCondition)

	if err != nil {
		return err
	}

	err = g.writeInstruction4(jpfl, chain.IfCondition.returnType().kind&KindSizeMask, 0)

	if err != nil {
		return err
	}

	seekFalse, err = g.Output.Seek(0, io.SeekCurrent)

	if err != nil {
		return err
	}

	seekFalse -= 4

	err = g.writeCodeBlock(chain.IfStatement)

	if err != nil {
		return err
	}

	if chain.hasElse || len(chain.ElifConditions) > 0 {
		err = g.writeInstruction4(jump, 0, 0)

		if err != nil {
			return err
		}

		seekEnd, err := g.Output.Seek(0, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekEnd -= 4

		seekEnds = append(seekEnds, seekEnd)
	}

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

	for i, condition := range chain.ElifConditions {
		statement := chain.ElifStatements[i]

		err := g.writeExpression(condition)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(jpfl, condition.returnType().kind&KindSizeMask, 0)

		if err != nil {
			return err
		}

		seekFalse, err = g.Output.Seek(0, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekFalse -= 4

		err = g.writeCodeBlock(statement)

		if err != nil {
			return err
		}

		if chain.hasElse || i < len(chain.ElifConditions)-1 {
			err = g.writeInstruction4(jump, 0, 0)

			if err != nil {
				return err
			}

			seekEnd, err := g.Output.Seek(0, io.SeekCurrent)

			if err != nil {
				return err
			}

			seekEnd -= 4
			seekEnds = append(seekEnds, seekEnd)
		}

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
		err := g.writeInstructionn(push, size, uint64(expression.value))

		if err != nil {
			return err
		}

	case FloatLit:
		size := expression.returnType().kind & KindSizeMask
		float, _ := strconv.ParseFloat(expression.value, int(size*8)) // at this point it should be a valid float

		var err error

		switch size {
		case Kind32:
			err = g.writeInstructionn(push, 4, uint64(math.Float32bits(float32(float))))
		case Kind64:
			err = g.writeInstructionn(push, 8, math.Float64bits(float))
		}

		if err != nil {
			return err
		}

	case StrLit:
		g.data[g.nextCompiletimeVarIdx] = []byte(expression.value)
		g.nextCompiletimeVarIdx++
		return nil // cant realy push a pointer yet but this should only be used by prts? so handle on that level?

	case CharLit:
		err := g.writeInstructionn(push, 4, uint64(expression.value))

		if err != nil {
			return err
		}

	case BoolLit:
		if expression.value {
			err := g.writeInstructionn(push, 4, 1)

			if err != nil {
				return err
			}
		} else {
			err := g.writeInstructionn(push, 4, 0)

			if err != nil {
				return err
			}
		}

	case Var:
		err := g.writeInstruction4(pshv, expression.typ.kind & KindSizeMask, g.vars[expression.name])

		if err != nil {
			return err
		}

	case Const:
		err := g.writeInstruction4(pshv, expression.typ.kind & KindSizeMask, g.vars[expression.name])

		if err != nil {
			return err
		}

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

				err = g.writeInstruction0(prti, arg.returnType().kind & KindSizeMask)

				if err != nil {
					return err
				}

			case KindFloat:
				err := g.writeExpression(arg)

				if err != nil {
					return err
				}

				err = g.writeInstruction0(prtf, arg.returnType().kind & KindSizeMask)

				if err != nil {
					return err
				}

			case KindString & KindTypeMask:
				var err error

				if aVar, ok := arg.(Var); ok {
					name := aVar.name

					err = g.writeInstruction4(prts, arg.returnType().kind & KindSizeMask, g.vars[name])
				} else if aConst, ok := arg.(Const); ok {
					name := aConst.name

					err = g.writeInstruction4(prts, arg.returnType().kind & KindSizeMask, g.vars[name])
				} else if aLit, ok := arg.(StrLit); ok {
					err = g.writeExpression(aLit)

					if err != nil {
						return err
					}

					err = g.writeInstruction4(prts, arg.returnType().kind & KindSizeMask, g.nextCompiletimeVarIdx - 1)
				}

				if err != nil {
					return err
				}
			}

			break
		}

		for i := len(expression.Args) - 1; i >= 0; i-- {
			arg := expression.Args[i]

			err := g.writeExpression(arg)

			if err != nil {
				return err
			}
		}

		err := g.writeInstruction4(call, 0, g.funcs[expression.fun.name])

		if err != nil {
			return err
		}

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
			err = g.writeInstruction0(adds, kind & KindSizeMask)
		} else {
			err = g.writeInstruction0(addf, kind & KindSizeMask)
		}

		if err != nil {
			return err
		}

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
			err = g.writeInstruction0(subs, kind & KindSizeMask)
		} else {
			err = g.writeInstruction0(subf, kind & KindSizeMask)
		}

		if err != nil {
			return err
		}

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
			err = g.writeInstruction0(muls, kind & KindSizeMask)
		} else {
			err = g.writeInstruction0(mulf, kind & KindSizeMask)
		}

		if err != nil {
			return err
		}

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
			err = g.writeInstruction0(divs, kind & KindSizeMask)
		} else {
			err = g.writeInstruction0(divf, kind & KindSizeMask)
		}

		if err != nil {
			return err
		}

	case BOMod:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(mods, binopnode.Lhs.returnType().kind & KindSizeMask)

		if err != nil {
			return err
		}

	case BOBinAnd:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		size := binopnode.returnType().kind & KindSizeMask

		err = g.writeInstruction0(band, size)

		if err != nil {
			return err
		}

	case BOBinOr:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		size := binopnode.returnType().kind & KindSizeMask

		err = g.writeInstruction0(borr, size)

		if err != nil {
			return err
		}

	case BOBinXor:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		size := binopnode.returnType().kind & KindSizeMask

		err = g.writeInstruction0(bxor, size)

		if err != nil {
			return err
		}

	case BOBoolAnd:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(jpfl, binopnode.Lhs.returnType().kind & KindSizeMask, 0)

		if err != nil {
			return err
		}

		seekFalse, err := g.Output.Seek(0, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekFalse -= 4

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(jptr, binopnode.Rhs.returnType().kind & KindSizeMask, 0)

		if err != nil {
			return err
		}

		seekTrue, err := g.Output.Seek(0, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekTrue -= 4

		err = g.writeInstructionn(push, 4, 0)

		if err != nil {
			return err
		}

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

		err = g.writeInstruction4(jump, 0, uint32(g.instructionIdx+2))

		if err != nil {
			return err
		}

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

		err = g.writeInstructionn(push, 4, 1)

		if err != nil {
			return err
		}

	case BOBoolOr:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(jptr, binopnode.Lhs.returnType().kind &KindSizeMask, 0)

		if err != nil {
			return err
		}

		seekTrue, err := g.Output.Seek(0, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekTrue -= 4

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(jpfl, binopnode.Lhs.returnType().kind &KindSizeMask, 0)

		if err != nil {
			return err
		}

		seekFalse, err := g.Output.Seek(0, io.SeekCurrent)

		if err != nil {
			return err
		}

		seekFalse -= 4

		err = g.writeInstructionn(push, 4, 1)

		if err != nil {
			return err
		}

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

		err = g.writeInstruction4(jump, 0, uint32(g.instructionIdx + 2))

		if err != nil {
			return err
		}

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

		err = g.writeInstructionn(push, 4, 0)

		if err != nil {
			return err
		}

	case BOShl:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		size := binopnode.returnType().kind & KindSizeMask
		err = g.writeInstruction0(bshl, size)

		if err != nil {
			return err
		}

	case BOShr:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		size := binopnode.returnType().kind & KindSizeMask

		err = g.writeInstruction0(bsrs, size)

		if err != nil {
			return err
		}

	case BOGt:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		if binopnode.Lhs.returnType().kind & KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().kind & KindSizeMask)
		} else if binopnode.Lhs.returnType().kind & KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().kind & KindSizeMask)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(isgt, binopnode.returnType().kind & KindSizeMask)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().kind & KindSizeMask
		curSize := binopnode.Lhs.returnType().kind & KindSizeMask

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

		if binopnode.Lhs.returnType().kind & KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().kind & KindSizeMask)
		} else if binopnode.Lhs.returnType().kind & KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().kind & KindSizeMask)
		}


		if err != nil {
			return err
		}

		err = g.writeInstruction0(islt, binopnode.returnType().kind & KindSizeMask)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().kind & KindSizeMask
		curSize := binopnode.Lhs.returnType().kind & KindSizeMask

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

		if binopnode.Lhs.returnType().kind & KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().kind & KindSizeMask)
		} else if binopnode.Lhs.returnType().kind & KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().kind & KindSizeMask)
		}


		if err != nil {
			return err
		}

		err = g.writeInstruction0(isge, binopnode.returnType().kind & KindSizeMask)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().kind & KindSizeMask
		curSize := binopnode.Lhs.returnType().kind & KindSizeMask

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

		if binopnode.Lhs.returnType().kind & KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().kind & KindSizeMask)
		} else if binopnode.Lhs.returnType().kind & KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().kind & KindSizeMask)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(isle, binopnode.returnType().kind & KindSizeMask)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().kind & KindSizeMask
		curSize := binopnode.Lhs.returnType().kind & KindSizeMask

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

		if binopnode.Lhs.returnType().kind & KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().kind & KindSizeMask)
		} else if binopnode.Lhs.returnType().kind & KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().kind & KindSizeMask)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(iseq, binopnode.returnType().kind & KindSizeMask)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().kind & KindSizeMask
		curSize := binopnode.Lhs.returnType().kind & KindSizeMask

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

		if binopnode.Lhs.returnType().kind & KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().kind & KindSizeMask)
		} else if binopnode.Lhs.returnType().kind & KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().kind & KindSizeMask)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(isne, binopnode.returnType().kind & KindSizeMask)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().kind & KindSizeMask
		curSize := binopnode.Lhs.returnType().kind & KindSizeMask

		g.castUnsigned(requiredSize, curSize)

	case BOAssign:
		lhs, ok := binopnode.Lhs.(Var)
		if !ok {
			panic("assigning to not variable???")
		}

		g.writeExpression(binopnode.Rhs)


		// TODO: this is the hackiest shit ever, should be solved when pointers exist
		if (lhs.returnType().kind & KindString & KindTypeMask) != 0 {
			break
		}

		size := lhs.returnType().kind & KindSizeMask

		err := g.writeInstruction4(popv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

	case BOPlusAssign:
		lhs, ok := binopnode.Lhs.(Var)
		if !ok {
			panic("assigning to not var??")
		}

		size := lhs.typ.kind & KindSizeMask

		err := g.writeInstruction4(pshv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch lhs.typ.kind & KindTypeMask {
		case KindInt:
			err = g.writeInstruction0(adds, size)
		case KindFloat:
			err = g.writeInstruction0(addf, size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction4(popv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

	case BODashAssign:
		lhs, ok := binopnode.Lhs.(Var)
		if !ok {
			panic("assigning to not var??")
		}

		size := lhs.typ.kind & KindSizeMask

		err := g.writeInstruction4(pshv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch lhs.typ.kind & KindTypeMask {
		case KindInt:
			err = g.writeInstruction0(subs, size)
		case KindFloat:
			err = g.writeInstruction0(subf, size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction4(popv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

	case BOStarAssign:
		lhs, ok := binopnode.Lhs.(Var)
		if !ok {
			panic("assigning to not var??")
		}

		size := lhs.typ.kind & KindSizeMask

		err := g.writeInstruction4(pshv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch lhs.typ.kind & KindTypeMask {
		case KindInt:
			err = g.writeInstruction0(muls, size)
		case KindFloat:
			err = g.writeInstruction0(mulf, size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction4(popv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

	case BOSlashAssign:
		lhs, ok := binopnode.Lhs.(Var)
		if !ok {
			panic("assigning to not var??")
		}

		size := lhs.typ.kind & KindSizeMask

		err := g.writeInstruction4(pshv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch lhs.typ.kind & KindTypeMask {
		case KindInt:
			err = g.writeInstruction0(divs, size)
		case KindFloat:
			err = g.writeInstruction0(divf, size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction4(popv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}


	case BoAndAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := lhs.typ.kind & KindSizeMask
		if !ok {
			panic("assigning to not var??")
		}

		err := g.writeInstruction4(pshv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(band, size)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(popv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

	case BoOrAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := lhs.typ.kind & KindSizeMask
		if !ok {
			panic("assigning to not var??")
		}

		err := g.writeInstruction4(pshv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(borr, size)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(popv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

	case BoXorAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := lhs.typ.kind & KindSizeMask
		if !ok {
			panic("assigning to not var??")
		}

		err := g.writeInstruction4(pshv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(bxor, size)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(popv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

	case BOShrAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := lhs.typ.kind & KindSizeMask
		if !ok {
			panic("assigning to not var??")
		}

		err := g.writeInstruction4(pshv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(bsrs, size)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(popv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

	case BOShlAssign:
		lhs, ok := binopnode.Lhs.(Var)
		size := lhs.typ.kind & KindSizeMask
		if !ok {
			panic("assigning to not var??")
		}

		err := g.writeInstruction4(pshv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(bshl, size)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(popv, size, g.vars[lhs.name])

		if err != nil {
			return err
		}

	default:
		panic("unreachable")
	}

	return nil
}

func (g *BytecodeGenerator) castUnsigned(requiredSize, currentSize Kind) (err error) {
	for currentSize > requiredSize {
		currentSize /= 2

		err = g.writeInstruction0(swap, currentSize)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(pops, currentSize)

		if err != nil {
			return err
		}
	}

	for currentSize < requiredSize {
		err = g.writeInstructionn(push, currentSize, 0)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(swap, currentSize)

		if err != nil {
			return err
		}

		currentSize *= 2
	}

	return nil
}


func (g *BytecodeGenerator) writeInstruction0(opcode Opcode, size Kind) error {
	_, err := g.Output.Write([]byte{byte(opcode), byte(size)})

	if err != nil {
		return err
	}

	g.bytesWritten += 2
	g.instructionIdx++

	return nil
}

func (g *BytecodeGenerator) writeInstruction4(opcode Opcode, size Kind, argument uint32) error {
	_, err := g.Output.Write([]byte{byte(opcode), byte(size)})

	if err != nil {
		return err
	}

	err = binary.Write(g.Output, binary.BigEndian, argument)

	if err != nil {
		return err
	}

	g.bytesWritten += 6
	g.instructionIdx++

	return nil
}

func (g *BytecodeGenerator) writeInstructionn(opcode Opcode, size Kind, argument uint64) error {
	_, err := g.Output.Write([]byte{byte(opcode), byte(size)})

	if err != nil {
		return err
	}

	switch size {
	case 1:
		err = binary.Write(g.Output, binary.BigEndian, uint8(argument))
	case 2:
		err = binary.Write(g.Output, binary.BigEndian, uint16(argument))
	case 4:
		err = binary.Write(g.Output, binary.BigEndian, uint32(argument))
	case 8:
		err = binary.Write(g.Output, binary.BigEndian, uint64(argument))
	}

	if err != nil {
		return err
	}

	g.bytesWritten += 2 + int(size)
	g.instructionIdx++

	return nil
}
