package bytecode

import (
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"math"
	"reflect"
	"slices"
	"strconv"
	"windlang/ast"
)

type varLocation struct {
	identifier ast.Identifier
	typ        ast.Type
	pointer    pointer // note offset for LocStack is relative to base
}

type generator struct {
	Output         io.WriteSeeker
	data           []byte
	instructionIdx int
	bytesWritten   int
	funcs          map[ast.Identifier]uint32
	vars           []varLocation
	baseOffset    uint64
	scratchLoc     uint64
}

func Generate(output io.WriteSeeker, code ast.CodeBlockNode) error {
	g := &generator{
		Output:         output,
		data:           make([]byte, 0),
		vars:           make([]varLocation, 0),
		funcs:          make(map[ast.Identifier]uint32),
		instructionIdx: 0,
		bytesWritten:   0,
		baseOffset:    0,
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

func (g *generator) writeGlobal(codeBlock ast.CodeBlockNode) error {
	_, exists := codeBlock.Scope.Funcs["main"]
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

	g.vars = slices.Grow(g.vars, len(codeBlock.Scope.Vars))
	for _, varDef := range codeBlock.Scope.Vars {
		g.vars = append(g.vars,
			varLocation{varDef.Name, varDef.Typ, pointer{locDataSection, 0, uint64(len(g.data))}},
		)
		g.data = append(g.data, make([]byte, varDef.Typ.Size())...)
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

func (g *generator) writeCodeBlock(codeBlock ast.CodeBlockNode) error {
	var err error

	addedVars := len(codeBlock.Scope.Vars)
	oldBase := g.baseOffset
	g.vars = slices.Grow(g.vars, addedVars)

	for _, varDef := range codeBlock.Scope.Vars {
		g.vars = append(g.vars,
			varLocation{varDef.Name, varDef.Typ, pointer{locStack, 0, g.baseOffset}},
		)

		size := varDef.Typ.Size()
		g.baseOffset += uint64(size)
	}

	err = g.writeStatements(codeBlock.Statements)

	if err != nil {
		return err
	}

	g.vars = g.vars[:len(g.vars)-addedVars]
	g.baseOffset = oldBase

	return nil
}

func (g *generator) writeStatements(statements []ast.AstNode) error {
	for _, node := range statements {
		switch node := node.(type) {
		case ast.ExpressionNode:
			err := g.writeExpression(node.Expr)

			if err != nil {
				return err
			}

			if node.Expr.ReturnType().Kind() != ast.KindVoid {
				err = g.writeInstruction0(pops, node.Expr.ReturnType().Size())

				if err != nil {
					return err
				}
			}

		case ast.VarNode:
			if node.Value == nil {
				break
			}

			err := g.varPointer(node.Var)

			if err != nil {
				return err
			}

			err = g.writeExpression(node.Value)

			if err != nil {
				return err
			}

			err = g.writeInstruction0(stor, node.Typ.Size())

			if err != nil {
				return err
			}

		case ast.FuncNode:
			err := g.writeFuncNode(node)

			if err != nil {
				return err
			}

		case ast.CodeBlockNode:
			err := g.writeCodeBlock(node)

			if err != nil {
				return err
			}

		case ast.IfChain:
			err := g.writeIfChain(node)

			if err != nil {
				return err
			}

		case ast.WhileNode:
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

			err = g.writeCodeBlock(node.Body)

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

		case ast.ReturnNode:
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

				err = g.writeInstruction0(stor, node.Expr.ReturnType().Size())

				if err != nil {
					return err
				}
			}

			err := g.writeInstruction0(rett, 0)

			if err != nil {
				return err
			}

		default:
			panic("Unimplemented ast.type: " + node.String())
		}
	}

	return nil
}

func (g *generator) writeFuncNode(fun ast.FuncNode) error {
	g.funcs[fun.Name] = uint32(g.instructionIdx)

	if g.baseOffset != 0 { // should be 0 since functions are declared on the top level
		fmt.Println(fun.Name, g.baseOffset)
		panic("assertion failed")
	}

	if fun.ReturnType.Kind() != ast.KindVoid {
		g.baseOffset = 8 // return value pointer
	}

	for i := len(fun.Args) - 1; i >= 0; i-- {
		arg := fun.Args[i]
		g.vars = append(g.vars,
			varLocation{arg.Name, arg.Typ, pointer{locStack, 0, g.baseOffset}},
		)

		size := arg.Typ.Size()
		g.baseOffset += uint64(size)
	}

	localBytes, scratchBytes := analyseStackUsage(fun.Body)

	g.scratchLoc = g.baseOffset + localBytes

	localBytes += scratchBytes

	localBytes = (localBytes+7)/8*8 // round up to multiple of 8

	for range localBytes/8 {
		err := g.writeInstructionn(push, 8, 0)

		if err != nil {
			return err
		}
	}

	err := g.writeCodeBlock(fun.Body)

	if err != nil {
		return err
	}

	g.baseOffset = 0
	g.scratchLoc = 0

	return nil
}

func analyseStackUsage(block ast.CodeBlockNode) (localBytes, scratchBytes uint64) {
	for _, varDef := range block.Scope.Vars {
		localBytes += uint64(varDef.Typ.Size())
	}

	var extraLocal uint64
	for _, node := range block.Statements {
		switch node := node.(type) {
		case ast.IfChain:

			scratchBytes = max(scratchBytes, analyseNeededScratch(node.IfCondition))

			statementLocals, statementScratch := analyseStackUsage(node.IfStatement)

			scratchBytes = max(scratchBytes, statementScratch)
			extraLocal = max(extraLocal, statementLocals)

			for i, condition := range node.ElifConditions {
				scratchBytes = max(scratchBytes, analyseNeededScratch(condition))

				statementLocals, statementScratch = analyseStackUsage(node.ElifStatements[i])

				extraLocal = max(extraLocal, statementLocals)
				scratchBytes = max(scratchBytes, statementScratch)
			}

			if node.HasElse {
				statementLocals, statementScratch = analyseStackUsage(node.ElseStatement)

				extraLocal = max(extraLocal, statementLocals)
				scratchBytes = max(scratchBytes, statementScratch)
			}

		case ast.WhileNode:

			scratchBytes = max(scratchBytes, analyseNeededScratch(node.Condition))

			statementLocals, statementScratch := analyseStackUsage(node.Body)

			extraLocal = max(extraLocal, statementLocals)
			scratchBytes = max(scratchBytes, statementScratch)

		case ast.CodeBlockNode:
			statementLocals, statementScratch := analyseStackUsage(node)

			extraLocal = max(extraLocal, statementLocals)
			scratchBytes = max(scratchBytes, statementScratch)

		case ast.ReturnNode:
			if node.Expr != nil {
				scratchBytes = max(scratchBytes, analyseNeededScratch(node.Expr))
			}

		case ast.ExpressionNode:
			scratchBytes = max(scratchBytes, analyseNeededScratch(node.Expr))
		}
	}

	localBytes += extraLocal

	return
}

func analyseNeededScratch(expr ast.Expression) (scratchBytes uint64) {
	return // always 0 for now
}

func (g *generator) writeIfChain(chain ast.IfChain) error {
	var seekEnds []int64
	var seekFalse int64

	err := g.writeExpression(chain.IfCondition)

	if err != nil {
		return err
	}

	err = g.writeInstruction4(jpfl, chain.IfCondition.ReturnType().Size(), 0)

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

	if chain.HasElse || len(chain.ElifConditions) > 0 {
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

		err = g.writeInstruction4(jpfl, condition.ReturnType().Size(), 0)

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

		if chain.HasElse || i < len(chain.ElifConditions)-1 {
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

	if chain.HasElse {
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

func (g *generator) writeExpression(expression ast.Expression) error {
	switch expression := expression.(type) {
	case ast.IntLit:
		size := expression.ReturnType().Size()
		err := g.writeInstructionn(push, size, uint64(expression.Value))

		if err != nil {
			return err
		}

	case ast.FloatLit:
		size := expression.ReturnType().Size()
		float := expression.Value

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

	case ast.StrLit:
		ptr := pointer{locDataSection, 0, uint64(len(g.data))}
		g.data = binary.BigEndian.AppendUint64(g.data, uint64(len(expression.Value)))
		g.data = append(g.data, []byte(expression.Value)...)

		err := g.writeInstructionn(push, 8, ptr.toUint64())

		if err != nil {
			return err
		}

	case ast.CharLit:
		err := g.writeInstructionn(push, 4, uint64(expression.Value))

		if err != nil {
			return err
		}

	case ast.BoolLit:
		if expression.Value {
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

	case ast.Var:
		err := g.varPointer(expression)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, expression.Typ.Size())

		if err != nil {
			return err
		}

	case ast.Func:
		// I beleive this is a function object?, definitely not implemented
		panic("not implemented")

	case ast.FuncCall:
		var err error

		if expression.Fun.Name == "print" || expression.Fun.Name == "println" {
			if len(expression.Args) != 1 {
				panic(fmt.Errorf("Exactly one argument expected for %s", expression.Fun.Name))
			}

			arg := expression.Args[0]

			err = g.writeExpression(arg)

			if err != nil {
				return err
			}

			switch arg.ReturnType().Kind() {
			case ast.KindInt:
				err = g.writeInstruction0(prti, arg.ReturnType().Size())

				if err != nil {
					return err
				}

			case ast.KindUint, ast.KindBool:
				err = g.writeInstruction0(prtu, arg.ReturnType().Size())

				if err != nil {
					return err
				}

			case ast.KindFloat:
				err = g.writeInstruction0(prtf, arg.ReturnType().Size())

				if err != nil {
					return err
				}

			case ast.KindString:
				if arg.ReturnType().Name() != "string" {
					panic("cannot print pointers")
				}

				err = g.writeInstruction0(prts, 0)

				if err != nil {
					return err
				}

			default:
				panic("cannot print kind " + strconv.Itoa(int(arg.ReturnType().Kind())))
			}

			break
		}

		if expression.ReturnType().Kind() != ast.KindVoid {
			err = g.writeInstructionn(push, expression.ReturnType().Size(), 0)

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

			err = g.writeInstructionn(push, 8, uint64(expression.ReturnType().Size()))

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

		err = g.writeInstruction4(call, 0, g.funcs[expression.Fun.Name])

		if err != nil {
			return err
		}

	case ast.Cast:
		switch expression.NewType.Kind() {
		case ast.KindInt:
			err := g.writeExpression(expression.Inner)

			if err != nil {
				return err
			}

			newSize, oldSize := expression.NewType.Size(), expression.Inner.ReturnType().Size()

			if expression.Inner.ReturnType().Kind() == ast.KindInt || newSize == oldSize {
				err = g.castSigned(newSize, oldSize)
			} else {
				// when casting from uint to int of different sizes 2 cases can occur:
				// sizeof uint > sizeof int: for casting to smaller size, signed and unsigned behave the same
				// sizeof uint < sizeof int: need to treat as unsigned cast ((0xff: uint8): int64) should be 255 not -1
				err = g.castUnsigned(newSize, oldSize)
			}

			if err != nil {
				return err
			}

		case ast.KindUint, ast.KindBool:
			err := g.writeExpression(expression.Inner)

			if err != nil {
				return err
			}

			err = g.castUnsigned(expression.NewType.Size(), expression.Inner.ReturnType().Size())

			if err != nil {
				return err
			}

		case ast.KindPointer:
			panic("should not happen")

		case ast.KindFloat:
			err := g.writeExpression(expression.Inner)

			if err != nil {
				return err
			}

			err = g.writeInstruction0(cvtf, expression.NewType.Size())

			if err != nil {
				return err
			}
		}

	case ast.StructIndex:
		switch base := expression.Base.(type) {
		case ast.Var:
			err := g.varPointer(base)

			if err != nil {
				return err
			}

			err = g.writeInstructionn(push, 8, uint64(expression.Offset))

			if err != nil {
				return err
			}

			err = g.writeInstruction0(addu, 8)

			if err != nil {
				return err
			}

			err = g.writeInstruction0(load, expression.Typ.Size())

			if err != nil {
				return err
			}
		default:
			panic("help me pwease")
		}

	case ast.BinaryOpNode:
		return g.generateBinaryOpNode(expression)

	case ast.UnaryOpNode:
		return g.generateUnaryOpNode(expression)

	default:
		panic("Unknow or unimplemented expression: " + reflect.TypeOf(expression).String())
	}

	return nil
}

func (g *generator) generateBinaryOpNode(binopnode ast.BinaryOpNode) error {
	if ast.BOCount != 28 {
		panic("Unary opperation enum length changed")
	}

	switch binopnode.Op {
	case ast.BOPlus:
		err := g.writeArithmetic(binopnode, adds, addu, addf)

		if err != nil {
			return err
		}

	case ast.BOMinus:
		err := g.writeArithmetic(binopnode, subs, subu, subf)

		if err != nil {
			return err
		}

	case ast.BOMul:
		err := g.writeArithmetic(binopnode, muls, mulu, mulf)

		if err != nil {
			return err
		}

	case ast.BODiv:
		err := g.writeArithmetic(binopnode, divs, divu, divf)

		if err != nil {
			return err
		}

	case ast.BOMod:
		err := g.writeArithmetic(binopnode, mods, modu, -1)

		if err != nil {
			return err
		}

	case ast.BOBinAnd:
		err := g.writeArithmetic(binopnode, band, band, -1)

		if err != nil {
			return err
		}

	case ast.BOBinOr:
		err := g.writeArithmetic(binopnode, borr, borr, -1)

		if err != nil {
			return err
		}

	case ast.BOBinXor:
		err := g.writeArithmetic(binopnode, bxor, bxor, -1)

		if err != nil {
			return err
		}

	case ast.BOBoolAnd:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(jpfl, binopnode.Lhs.ReturnType().Size(), 0)

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

		err = g.writeInstruction4(jptr, binopnode.Rhs.ReturnType().Size(), 0)

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

	case ast.BOBoolOr:
		err := g.writeExpression(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction4(jptr, binopnode.Lhs.ReturnType().Size(), 0)

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

		err = g.writeInstruction4(jpfl, binopnode.Lhs.ReturnType().Size(), 0)

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

	case ast.BOShl:
		err := g.writeArithmetic(binopnode, bshl, bshl, -1)

		if err != nil {
			return err
		}

	case ast.BOShr:
		err := g.writeArithmetic(binopnode, bsrs, bsru, -1)

		if err != nil {
			return err
		}

	case ast.BOGt:
		err := g.writeBoolExpression(binopnode, isgt)

		if err != nil {
			return err
		}

	case ast.BOLt:
		err := g.writeBoolExpression(binopnode, islt)

		if err != nil {
			return err
		}

	case ast.BOGtEq:
		err := g.writeBoolExpression(binopnode, isge)

		if err != nil {
			return err
		}

	case ast.BOLtEq:
		err := g.writeBoolExpression(binopnode, isle)

		if err != nil {
			return err
		}

	case ast.BOEquals:
		err := g.writeBoolExpression(binopnode, iseq)

		if err != nil {
			return err
		}

	case ast.BONotEqual:
		err := g.writeBoolExpression(binopnode, isne)

		if err != nil {
			return err
		}

	case ast.BOAssign:
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(stor, binopnode.Rhs.ReturnType().Size())

		if err != nil {
			return err
		}

	case ast.BOPlusAssign:
		typ := binopnode.Lhs.ReturnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch typ.Kind() {
		case ast.KindInt:
			err = g.writeInstruction0(adds, typ.Size())
		case ast.KindUint:
			err = g.writeInstruction0(addu, typ.Size())
		case ast.KindFloat:
			err = g.writeInstruction0(addf, typ.Size())
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(stor, typ.Size())

		if err != nil {
			return err
		}

	case ast.BODashAssign:
		typ := binopnode.Lhs.ReturnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch typ.Kind() {
		case ast.KindInt:
			err = g.writeInstruction0(subs, typ.Size())
		case ast.KindUint:
			err = g.writeInstruction0(subu, typ.Size())
		case ast.KindFloat:
			err = g.writeInstruction0(subf, typ.Size())
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(stor, typ.Size())

		if err != nil {
			return err
		}

	case ast.BOStarAssign:
		typ := binopnode.Lhs.ReturnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch typ.Kind() {
		case ast.KindInt:
			err = g.writeInstruction0(muls, typ.Size())
		case ast.KindUint:
			err = g.writeInstruction0(mulu, typ.Size())
		case ast.KindFloat:
			err = g.writeInstruction0(mulf, typ.Size())
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(stor, typ.Size())

		if err != nil {
			return err
		}

	case ast.BOSlashAssign:
		typ := binopnode.Lhs.ReturnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		switch typ.Kind() {
		case ast.KindInt:
			err = g.writeInstruction0(divs, typ.Size())
		case ast.KindUint:
			err = g.writeInstruction0(divu, typ.Size())
		case ast.KindFloat:
			err = g.writeInstruction0(divf, typ.Size())
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

	case ast.BOAndAssign:
		typ := binopnode.Lhs.ReturnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(band, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

	case ast.BOOrAssign:
		typ := binopnode.Lhs.ReturnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(borr, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

	case ast.BOXorAssign:
		typ := binopnode.Lhs.ReturnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(bxor, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

	case ast.BOShrAssign:
		typ := binopnode.Lhs.ReturnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		if typ.Kind() == ast.KindInt {
			err = g.writeInstruction0(bsrs, typ.Size())
		} else if typ.Kind() == ast.KindUint {
			err = g.writeInstruction0(bsru, typ.Size())
		} else {
			panic("unreachable")
		}

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

	case ast.BOShlAssign:
		typ := binopnode.Lhs.ReturnType()
		err := g.writeAssignLhs(binopnode.Lhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(dupe, 8)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeExpression(binopnode.Rhs)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(bshl, typ.Size())

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, typ.Size())

		if err != nil {
			return err
		}
	default:
		panic("unreachable")
	}

	return nil
}

func (g *generator) generateUnaryOpNode(uo ast.UnaryOpNode) error {
	if ast.UOCount != 6 {
		panic("Unary opperation enum length changed")
	}

	switch uo.Op {
	case ast.UOPlus:
		return g.writeExpression(uo.Expression)

	case ast.UONegative:
		err := g.writeExpression(uo.Expression)

		if err != nil {
			return err
		}

		ReturnType := uo.Expression.ReturnType()

		if ReturnType.Kind() == ast.KindInt {
			err = g.writeInstruction0(negs, ReturnType.Size())
		} else if ReturnType.Kind() == ast.KindFloat {
			err = g.writeInstruction0(negf, ReturnType.Size())
		} else {
			panic("Unknown type for negate")
		}

		if err != nil {
			return err
		}

	case ast.UOBoolNot:
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

	case ast.UOBinNot:
		err := g.writeExpression(uo.Expression)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(bnot, uo.Expression.ReturnType().Size())

		if err != nil {
			return err
		}

	case ast.UORef:
		if expr, ok := uo.Expression.(ast.Var); ok {
			err := g.varPointer(expr)

			if err != nil {
				return err
			}
		} else if expr, ok := uo.Expression.(ast.StructIndex); ok {
			base, ok := expr.Base.(ast.Var)

			if !ok {
				return errors.New("ERROR: can only take a reference to a variable")
			}

			err := g.varPointer(base)

			if err != nil {
				return err
			}

			err = g.writeInstructionn(push, 8, uint64(expr.Offset))

			if err != nil {
				return err
			}

			err = g.writeInstruction0(addu, 8)

			if err != nil {
				return err
			}

		} else {
			panic("unreachable")
		}

	case ast.UODeref:
		retTyp, innerTyp := uo.ReturnType(), uo.Expression.ReturnType().(ast.PointerType).Inner
		if retTyp.Kind() != innerTyp.Kind() || retTyp.Size() != innerTyp.Size() {
			panic("assertion failed")
		}

		err := g.writeExpression(uo.Expression)

		if err != nil {
			return err
		}

		err = g.writeInstruction0(load, retTyp.Size())

		if err != nil {
			return err
		}

	default:
		panic("unreachable")
	}

	return nil
}

func (g *generator) writeArithmetic(binopnode ast.BinaryOpNode,
	signedOp, unsignedOp, floatOp Opcode) error {
	err := g.writeExpression(binopnode.Lhs)

	if err != nil {
		return err
	}

	err = g.writeExpression(binopnode.Rhs)

	if err != nil {
		return err
	}

	typ := binopnode.Lhs.ReturnType()

	switch typ.Kind() {
	case ast.KindInt:
		err = g.writeInstruction0(signedOp, typ.Size())
	case ast.KindUint:
		err = g.writeInstruction0(unsignedOp, typ.Size())
	case ast.KindFloat:
		err = g.writeInstruction0(floatOp, typ.Size())
	default:
		panic("unreachable")
	}

	return err
}

func (g *generator) writeBoolExpression(binopnode ast.BinaryOpNode, instruction Opcode) error {
	err := g.writeExpression(binopnode.Lhs)

	if err != nil {
		return err
	}

	err = g.writeExpression(binopnode.Rhs)

	if err != nil {
		return err
	}

	typ := binopnode.Lhs.ReturnType()

	if typ.Kind() == ast.KindInt {
		err = g.writeInstruction0(cmps, typ.Size())
	} else if typ.Kind() == ast.KindUint {
		err = g.writeInstruction0(cmpu, typ.Size())
	} else if typ.Kind() == ast.KindFloat {
		err = g.writeInstruction0(cmpf, typ.Size())
	} else {
		panic("uncomparable types")
	}

	if err != nil {
		return err
	}

	err = g.writeInstruction0(instruction, binopnode.ReturnType().Size())

	if err != nil {
		return err
	}

	requiredSize := binopnode.ReturnType().Size()
	curSize := binopnode.Lhs.ReturnType().Size()

	err = g.castUnsigned(requiredSize, curSize)

	return err
}

func (g *generator) castUnsigned(requiredSize, currentSize int) (err error) {
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

func (g *generator) castSigned(requiredSize, currentSize int) (err error) {
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
		currentSize *= 2
		err = g.writeInstruction0(sgne, currentSize)

		if err != nil {
			return err
		}
	}

	return nil
}

func (g *generator) writeAssignLhs(lhs ast.Expression) error {
	derefCount := 0

loop:
	for true {
		switch lhsTyp := lhs.(type) {
		case ast.Var:
			err := g.varPointer(lhsTyp)

			if err != nil {
				return err
			}

			break loop
		case ast.StructIndex:
			base, ok := lhsTyp.Base.(ast.Var)

			if !ok {
				panic("Assigning to not var?")
			}

			err := g.varPointer(base)

			if err != nil {
				return err
			}

			err = g.writeInstructionn(push, 8, uint64(lhsTyp.Offset))

			if err != nil {
				return err
			}

			err = g.writeInstruction0(addu, 8)

			if err != nil {
				return err
			}

			break loop
		case ast.UnaryOpNode:
			if lhsTyp.Op != ast.UODeref {
				panic("assertion failed")
			}

			derefCount++

			lhs = lhsTyp.Expression

		default:
			panic("assigning to not variable???")
		}
	}

	for i := range derefCount {
		if i < derefCount-1 {
			err := g.writeInstruction0(load, 8)

			if err != nil {
				return err
			}
		} else {
			err := g.writeInstruction0(load, lhs.ReturnType().Size())

			if err != nil {
				return err
			}
		}
	}

	return nil
}

func (g *generator) writeInstruction0(opcode Opcode, size int) error {
	_, err := g.Output.Write([]byte{byte(opcode), byte(size)})

	if err != nil {
		return err
	}

	g.bytesWritten += 2
	g.instructionIdx++

	return nil
}

func (g *generator) writeInstruction4(opcode Opcode, size int, argument uint32) error {
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

func (g *generator) writeInstructionn(opcode Opcode, size int, argument uint64) error {
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

func (g *generator) findVar(identifier ast.Identifier) int {
	for i := len(g.vars) - 1; i >= 0; i-- {
		if g.vars[i].identifier == identifier {
			return i
		}
	}

	return -1
}

func (g *generator) varPointer(variable ast.Var) error {
	var varLoc varLocation

	for i := len(g.vars) - 1; i >= 0; i-- {
		cur := g.vars[i]
		if cur.identifier == variable.Name {
			varLoc = cur
			goto found
		}
	}

	panic("variable ´" + variable.Name + "´ not found")

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

func (g *generator) scratchPointer() error {
	err := g.writeInstruction0(base, 0)

	if err != nil {
		return err
	}

	err = g.writeInstructionn(push, 8, g.scratchLoc)

	if err != nil {
		return err
	}

	return g.writeInstruction0(addu, 8)
}
