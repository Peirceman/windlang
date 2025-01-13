package main

import (
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"math"
	"slices"
	"strconv"
)

type VarLocation struct {
	identifier Identifier
	typ        Type
	pointer    bytecodePointer // note offset for LocStack is relative to base
}

type BytecodeGenerator struct {
	Output         io.WriteSeeker
	data           []byte
	instructionIdx int
	bytesWritten   int
	funcs          map[Identifier]uint32
	vars           []VarLocation
	baseOffset     uint64
}

func GenerateBytecode(output io.WriteSeeker, code CodeBlockNode) error {
	g := &BytecodeGenerator{
		Output:         output,
		data:           make([]byte, 0),
		vars:           make([]VarLocation, 0),
		funcs:          make(map[Identifier]uint32),
		instructionIdx: 0,
		bytesWritten:   0,
		baseOffset:     0,
	}

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

	err = g.writeGlobal(code)

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

	_, err = g.Output.Write([]byte{
		byte(len(g.data) >> (3 * 8)),
		byte(len(g.data) >> (2 * 8)),
		byte(len(g.data) >> (1 * 8)),
		byte(len(g.data) >> (0 * 8)),
	})

	if err != nil {
		return err
	}

	_, err = g.Output.Write(g.data)

	if err != nil {
		return err
	}

	return nil
}

func (g *BytecodeGenerator) writeGlobal(codeBlock CodeBlockNode) error {
	_, exists := codeBlock.scope.Funcs["main"]
	if !exists {
		return errors.New("Error generating bytecode: no main function found")
	}

	err := g.writeInstruction4(call, 0, 0)

	if err != nil {
		return err
	}

	seek, err := g.Output.Seek(0, io.SeekCurrent)

	if err != nil {
		return err
	}

	seek -= 4

	err = g.writeInstructionn(push, 8, 0)

	if err != nil {
		return err
	}

	err = g.writeInstruction0(exit, 8)

	if err != nil {
		return err
	}

	g.vars = slices.Grow(g.vars, len(codeBlock.scope.vars)+len(codeBlock.scope.consts))
	for _, varDef := range codeBlock.scope.vars {
		g.vars = append(g.vars,
			VarLocation{varDef.name, varDef.typ, bytecodePointer{locDataSection, 0, uint64(len(g.data))}},
		)
		g.data = append(g.data, make([]byte, varDef.typ.size)...)
	}

	for _, constDef := range codeBlock.scope.consts {
		g.vars = append(g.vars,
			VarLocation{constDef.name, constDef.typ, bytecodePointer{locDataSection, 0, uint64(len(g.data))}},
		)
		g.data = append(g.data, make([]byte, constDef.typ.size)...)
	}

	err = g.writeStatements(codeBlock.Statements)

	if err != nil {
		return err
	}

	_, err = g.Output.Seek(seek, io.SeekStart)

	if err != nil {
		return err
	}

	err = binary.Write(g.Output, binary.BigEndian, g.funcs["main"])

	if err != nil {
		return err
	}

	_, err = g.Output.Seek(0, io.SeekEnd)

	if err != nil {
		return err
	}

	return nil
}

func (g *BytecodeGenerator) writeCodeBlock(codeBlock CodeBlockNode) error {
	var err error

	addedVars := len(codeBlock.scope.vars) + len(codeBlock.scope.consts)
	oldBase := g.baseOffset
	g.vars = slices.Grow(g.vars, addedVars)

	for _, varDef := range codeBlock.scope.vars {
		g.vars = append(g.vars,
			VarLocation{varDef.name, varDef.typ, bytecodePointer{locStack, 0, g.baseOffset}},
		)

		size := varDef.typ.size
		g.baseOffset += uint64(size)

		err = g.writeInstructionn(push, size, 0)

		if err != nil {
			return err
		}
	}

	for _, constDef := range codeBlock.scope.consts {
		g.vars = append(g.vars,
			VarLocation{constDef.name, constDef.typ, bytecodePointer{locStack, 0, g.baseOffset}},
		)

		size := constDef.typ.size
		g.baseOffset += uint64(size)

		err = g.writeInstructionn(push, size, 0)

		if err != nil {
			return err
		}
	}

	err = g.writeStatements(codeBlock.Statements)

	if err != nil {
		return err
	}

	g.vars = g.vars[:len(g.vars)-addedVars]
	g.baseOffset = oldBase

	for _, varDef := range codeBlock.scope.vars {
		size := varDef.typ.size

		err = g.writeInstruction0(pops, size)

		if err != nil {
			return err
		}
	}

	for _, constDef := range codeBlock.scope.consts {
		size := constDef.typ.size

		err = g.writeInstruction0(pops, size)

		if err != nil {
			return err
		}
	}

	return nil
}

func (g *BytecodeGenerator) writeStatements(statements []AstNode) error {
	for _, node := range statements {
		switch node := node.(type) {
		case ExpressionNode:
			err := g.writeExpression(node.Expr)

			if err != nil {
				return err
			}

			if node.Expr.returnType().kind != KindVoid {
				err = g.writeInstruction0(pops, node.Expr.returnType().size)

				if err != nil {
					return err
				}
			}

		case ConstNode:
			if node.Value == nil {
				break
			}

			err := g.varPointer(node.name)

			if err != nil {
				return err
			}

			err = g.writeExpression(node.Value)

			if err != nil {
				return err
			}

			err = g.writeInstruction0(stor, node.typ.size)

			if err != nil {
				return err
			}

		case VarNode:
			if node.Value == nil {
				break
			}

			err := g.varPointer(node.name)

			if err != nil {
				return err
			}

			err = g.writeExpression(node.Value)

			if err != nil {
				return err
			}

			err = g.writeInstruction0(stor, node.typ.size)

			if err != nil {
				return err
			}

		case FuncNode:
			g.funcs[node.name] = uint32(g.instructionIdx)
			if g.baseOffset != 0 { // should be 0 since functions are declared on the top level
				fmt.Println(node.name, g.baseOffset)
				panic("assertion failed")
			}

			if node.returnType.kind != KindVoid {
				g.baseOffset = 8 // return value pointer
			}

			for i := len(node.Args) - 1; i >= 0; i-- {
				arg := node.Args[i]
				g.vars = append(g.vars,
					VarLocation{arg.name, arg.typ, bytecodePointer{locStack, 0, g.baseOffset}},
				)

				size := arg.typ.size
				g.baseOffset += uint64(size)
			}

			err := g.writeCodeBlock(node.Body)

			if err != nil {
				return err
			}

			g.baseOffset = 0

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

		case WhileNode:
			conditionIdx := uint32(g.instructionIdx)

			err := g.writeExpression(node.Condition)

			if err != nil {
				return err
			}

			err = g.writeInstruction4(jpfl, 4, 0)

			if err != nil {
				return err
			}

			seekFalse, err := g.Output.Seek(0, io.SeekCurrent)

			if err != nil {
				return err
			}

			seekFalse -= 4

			err = g.writeCodeBlock(node.Loop)

			if err != nil {
				return err
			}

			err = g.writeInstruction4(jump, 4, conditionIdx)

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

		case ReturnNode:
			if node.Expr != nil {
				err := g.writeInstruction0(base, 0)

				if err != nil {
					return err
				}

				err = g.writeInstruction0(load, 8)

				if err != nil {
					return err
				}

				err = g.writeExpression(node.Expr)

				if err != nil {
					return err
				}

				err = g.writeInstruction0(stor, node.Expr.returnType().size)

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

	err = g.writeInstruction4(jpfl, chain.IfCondition.returnType().size, 0)

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

		err = g.writeInstruction4(jpfl, condition.returnType().size, 0)

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
		size := expression.returnType().size
		err := g.writeInstructionn(push, size, uint64(expression.value))

		if err != nil {
			return err
		}

	case FloatLit:
		size := expression.returnType().size
		float, _ := strconv.ParseFloat(expression.value, int(size*8)) // at this point it should be a valid float

		var err error

		switch size {
		case 4:
			err = g.writeInstructionn(push, 4, uint64(math.Float32bits(float32(float))))
		case 8:
			err = g.writeInstructionn(push, 8, math.Float64bits(float))
		}

		if err != nil {
			return err
		}

	case StrLit:
		ptr := bytecodePointer{locDataSection, 0, uint64(len(g.data))}
		g.data = binary.BigEndian.AppendUint64(g.data, uint64(len(expression.value)))
		g.data = append(g.data, []byte(expression.value)...)

		err := g.writeInstructionn(push, 8, ptr.toUint64())

		if err != nil {
			return err
		}

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
		err := g.varPointer(expression.name)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, expression.typ.size)

		if err != nil {
			return err
		}

	case Const:
		err := g.varPointer(expression.name)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, expression.typ.size)

		if err != nil {
			return err
		}

	case Func:
		// I beleive this is a function object?, definitely not implemented
		panic("not implemented")

	case FuncCall:
		var err error

		if expression.fun.name == "print" || expression.fun.name == "println" {
			if len(expression.Args) != 1 {
				panic(fmt.Errorf("Exactly one argument expected for %s", expression.fun.name))
			}

			arg := expression.Args[0]

			err = g.writeExpression(arg)

			if err != nil {
				return err
			}

			switch arg.returnType().kind {
			case KindInt, KindBool:
				err = g.writeInstruction0(prti, arg.returnType().size)

				if err != nil {
					return err
				}

			case KindFloat:
				err = g.writeInstruction0(prtf, arg.returnType().size)

				if err != nil {
					return err
				}

			case KindString:
				if arg.returnType().name != "string" {
					panic("cannot print pointers")
				}

				err = g.writeInstruction0(prts, 0)

				if err != nil {
					return err
				}
			}

			break
		}

		if expression.returnType().kind != KindVoid {
			err = g.writeInstructionn(push, expression.returnType().size, 0)

			if err != nil {
				return err
			}

			err = g.writeInstruction0(farg, 0)

			if err != nil {
				return err
			}

			err = g.writeInstruction0(sptr, 0)

			if err != nil {
				return err
			}

			err = g.writeInstructionn(push, 8, uint64(expression.returnType().size))

			if err != nil {
				return err
			}

			err = g.writeInstruction0(subu, 8)

			if err != nil {
				return err
			}
		} else {
			err = g.writeInstruction0(farg, 0)

			if err != nil {
				return err
			}
		}

		for i := len(expression.Args) - 1; i >= 0; i-- {
			arg := expression.Args[i]

			err := g.writeExpression(arg)

			if err != nil {
				return err
			}
		}

		err = g.writeInstruction4(call, 0, g.funcs[expression.fun.name])

		if err != nil {
			return err
		}

	case BinaryOpNode:
		return g.generateBinaryOpNode(expression)

	case UnaryOpNode:
		return g.generateUnaryOpNode(expression)
	}

	return nil
}

func (g *BytecodeGenerator) generateBinaryOpNode(binopnode BinaryOpNode) error {
	if BOCount != 28 {
		panic("Unary opperation enum length changed")
	}

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

		typ := binopnode.Lhs.returnType()
		if typ.kind == KindInt {
			err = g.writeInstruction0(adds, typ.size)
		} else {
			err = g.writeInstruction0(addf, typ.size)
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

		typ := binopnode.Lhs.returnType()
		if typ.kind == KindInt {
			err = g.writeInstruction0(subs, typ.size)
		} else {
			err = g.writeInstruction0(subf, typ.size)
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

		typ := binopnode.Lhs.returnType()
		if typ.kind == KindInt {
			err = g.writeInstruction0(muls, typ.size)
		} else {
			err = g.writeInstruction0(mulf, typ.size)
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

		typ := binopnode.Lhs.returnType()
		if typ.kind == KindInt {
			err = g.writeInstruction0(divs, typ.size)
		} else {
			err = g.writeInstruction0(divf, typ.size)
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

		err = g.writeInstruction0(mods, binopnode.Lhs.returnType().size)

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

		size := binopnode.returnType().size

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

		size := binopnode.returnType().size

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

		size := binopnode.returnType().size

		err = g.writeInstruction0(bxor, size)

		if err != nil {
			return err
		}

	case BOBoolAnd:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(jpfl, binopnode.Lhs.returnType().size, 0)

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

		err = g.writeInstruction4(jptr, binopnode.Rhs.returnType().size, 0)

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

		err = g.writeInstruction4(jptr, binopnode.Lhs.returnType().size, 0)

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

		err = g.writeInstruction4(jpfl, binopnode.Lhs.returnType().size, 0)

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

		err = g.writeInstruction4(jump, 0, uint32(g.instructionIdx+2))

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

		size := binopnode.returnType().size
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

		size := binopnode.returnType().size

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

		if binopnode.Lhs.returnType().kind&KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().size)
		} else if binopnode.Lhs.returnType().kind&KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(isgt, binopnode.returnType().size)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().size
		curSize := binopnode.Lhs.returnType().size

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

		if binopnode.Lhs.returnType().kind&KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().size)
		} else if binopnode.Lhs.returnType().kind&KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(islt, binopnode.returnType().size)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().size
		curSize := binopnode.Lhs.returnType().size

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

		if binopnode.Lhs.returnType().kind&KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().size)
		} else if binopnode.Lhs.returnType().kind&KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(isge, binopnode.returnType().size)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().size
		curSize := binopnode.Lhs.returnType().size

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

		if binopnode.Lhs.returnType().kind&KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().size)
		} else if binopnode.Lhs.returnType().kind&KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(isle, binopnode.returnType().size)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().size
		curSize := binopnode.Lhs.returnType().size

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

		if binopnode.Lhs.returnType().kind&KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().size)
		} else if binopnode.Lhs.returnType().kind&KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(iseq, binopnode.returnType().size)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().size
		curSize := binopnode.Lhs.returnType().size

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

		if binopnode.Lhs.returnType().kind&KindInt != 0 {
			err = g.writeInstruction0(cmps, binopnode.Lhs.returnType().size)
		} else if binopnode.Lhs.returnType().kind&KindFloat != 0 {
			err = g.writeInstruction0(cmpf, binopnode.Lhs.returnType().size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(isne, binopnode.returnType().size)

		if err != nil {
			return err
		}

		requiredSize := binopnode.returnType().size
		curSize := binopnode.Lhs.returnType().size

		g.castUnsigned(requiredSize, curSize)

	case BOAssign:
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(stor, binopnode.Rhs.returnType().size)

		if err != nil {
			return err
		}

	case BOPlusAssign:
		typ := binopnode.Lhs.returnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch typ.kind {
		case KindInt:
			err = g.writeInstruction0(adds, typ.size)
		case KindFloat:
			err = g.writeInstruction0(addf, typ.size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(stor, typ.size)

		if err != nil {
			return err
		}

	case BODashAssign:
		typ := binopnode.Lhs.returnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch typ.kind {
		case KindInt:
			err = g.writeInstruction0(subs, typ.size)
		case KindFloat:
			err = g.writeInstruction0(subf, typ.size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(stor, typ.size)

		if err != nil {
			return err
		}

	case BOStarAssign:
		typ := binopnode.Lhs.returnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch typ.kind {
		case KindInt:
			err = g.writeInstruction0(muls, typ.size)
		case KindFloat:
			err = g.writeInstruction0(mulf, typ.size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(stor, typ.size)

		if err != nil {
			return err
		}

	case BOSlashAssign:
		typ := binopnode.Lhs.returnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch typ.kind {
		case KindInt:
			err = g.writeInstruction0(divs, typ.size)
		case KindFloat:
			err = g.writeInstruction0(divf, typ.size)
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

	case BoAndAssign:
		typ := binopnode.Lhs.returnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(band, typ.size)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

	case BoOrAssign:
		typ := binopnode.Lhs.returnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(borr, typ.size)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

	case BoXorAssign:
		typ := binopnode.Lhs.returnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(bxor, typ.size)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

	case BOShrAssign:
		typ := binopnode.Lhs.returnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(bsrs, typ.size)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

	case BOShlAssign:
		typ := binopnode.Lhs.returnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(bshl, typ.size)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.size)

		if err != nil {
			return err
		}
	default:
		panic("unreachable")
	}

	return nil
}

func (g *BytecodeGenerator) generateUnaryOpNode(uo UnaryOpNode) error {
	if UOCount != 6 {
		panic("Unary opperation enum length changed")
	}

	switch uo.Op {
	case UOPlus:
		return g.writeExpression(uo.Expression)
	case UONegative:
		err := g.writeExpression(uo.Expression)

		if err != nil {
			return err
		}

		returnType := uo.Expression.returnType()

		if returnType.kind == KindInt {
			err = g.writeInstruction0(negs, returnType.size)
		} else if returnType.kind == KindFloat {
			err = g.writeInstruction0(negf, returnType.size)
		} else {
			panic("Unknown type for negate")
		}

		if err != nil {
			return err
		}

	case UOBoolNot:
		err := g.writeExpression(uo.Expression)

		if err != nil {
			return err
		}

		err = g.writeInstructionn(push, 4, 0)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(cmpu, 4)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(iseq, 4)

		if err != nil {
			return err
		}

	case UOBinNot:
		err := g.writeExpression(uo.Expression)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(bnot, uo.Expression.returnType().size)

		if err != nil {
			return err
		}

	case UORef:
		switch expr := uo.Expression.(type) {
		case Var:
			err := g.varPointer(expr.name)

			if err != nil {
				return err
			}

		case Const:
			err := g.varPointer(expr.name)

			if err != nil {
				return err
			}

		default:
			panic("unreachable")
		}

	case UODeref:
		retTyp, innerTyp := uo.returnType(), *uo.Expression.returnType().inner
		if retTyp.kind != innerTyp.kind || retTyp.size != innerTyp.size {
			panic("assertion failed")
		}

		err := g.writeExpression(uo.Expression)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, retTyp.size)

		if err != nil {
			return err
		}

	default:
		panic("unreachable")
	}

	return nil
}

func (g *BytecodeGenerator) castUnsigned(requiredSize, currentSize int) (err error) {
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

func (g *BytecodeGenerator) writeInstruction0(opcode Opcode, size int) error {
	_, err := g.Output.Write([]byte{byte(opcode), byte(size)})

	if err != nil {
		return err
	}

	g.bytesWritten += 2
	g.instructionIdx++

	return nil
}

func (g *BytecodeGenerator) writeInstruction4(opcode Opcode, size int, argument uint32) error {
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

func (g *BytecodeGenerator) writeInstructionn(opcode Opcode, size int, argument uint64) error {
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

func (g *BytecodeGenerator) findVar(identifier Identifier) int {
	for i := len(g.vars) - 1; i >= 0; i-- {
		if g.vars[i].identifier == identifier {
			return i
		}
	}

	return -1
}

func (g *BytecodeGenerator) varPointer(identifier Identifier) error {
	var varLoc VarLocation

	for i := len(g.vars) - 1; i >= 0; i-- {
		cur := g.vars[i]
		if cur.identifier == identifier {
			varLoc = cur
			goto found
		}
	}

	panic("variable ´" + identifier + "´ not found")

found:
	switch varLoc.pointer.location {
	case locDataSection: // global var
		err := g.writeInstructionn(push, 8, varLoc.pointer.toUint64())

		if err != nil {
			return err
		}
	case locStack: // local var
		err := g.writeInstruction0(base, 0)

		if err != nil {
			return err
		}
		err = g.writeInstructionn(push, 8, varLoc.pointer.byteOffset)

		if err != nil {
			return err
		}
		err = g.writeInstruction0(addu, 8)

		if err != nil {
			return err
		}
	case locAlloc:
		fallthrough // dynamically allocated pointer/array, already stored in local var
	default:
		panic("unreachable")
	}

	return nil
}

func (g *BytecodeGenerator) writeAssignLhs(lhs Expression) error{
	derefCount := 0

	for true {
		if lhs, ok := lhs.(Var); ok {
			err := g.varPointer(lhs.name)

			if err != nil {
				return err
			}

			break
		}

		if lhsUnOp, ok := lhs.(UnaryOpNode); ok {
			if lhsUnOp.Op != UODeref {
				panic("assertion failed")
			}

			derefCount++

			lhs = lhsUnOp.Expression

			continue
		}

		panic("assigning to not variable???")
	}

	for i := range derefCount {
		if i < derefCount-1 {
			err := g.writeInstruction0(load, 8)

			if err != nil {
				return err
			}
		} else {
			err := g.writeInstruction0(load, lhs.returnType().size)

			if err != nil {
				return err
			}
		}
	}

	return nil
}
