package bytecode

import (
	"container/list"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"math"
	"strconv"
)

type pointerLocation uint8

const (
	locDataSection pointerLocation = iota
	locStack
	locAlloc
)

type pointer struct {
	location   pointerLocation
	index      uint16
	byteOffset uint64 // only low 5 bytes in case of allocation else low 7 bytes
}

type linkedList struct {
	i    uint16
	next *linkedList
}

type callFrame struct {
	instruction uint32
	basePointer pointer
}

type Interpreter struct {
	instructions []Instruction
	Data         []byte
	Stack        []byte
	freeIndexes  *linkedList
	allocations  [][]byte
	emptySlots   []int
	callStack    []callFrame
	fargBase     *pointer // optional
}

func pointerFromUint64(i uint64) pointer {
	result := pointer{}
	result.location = pointerLocation(i >> (7 * 8))

	if result.location == locAlloc {
		result.index = uint16((i >> (5 * 8)) & ((1 << (2 * 8)) - 1))
		result.byteOffset = i & ((1 << (5 * 8)) - 1)
	} else {
		result.byteOffset = i & ((1 << (7 * 8)) - 1)
	}

	return result
}

func (p pointer) toUint64() uint64 {
	return (uint64(p.location) << (7 * 8)) | (uint64(p.index) << (5 * 8)) | p.byteOffset
}

func InterpeterFromReader(r io.Reader) (*Interpreter, error) {
	list.New()
	data, err := io.ReadAll(r)
	if err != nil {
		return &Interpreter{}, err
	}

	return InterpeterFromRawBytes(data)
}

func InterpeterFromRawBytes(data []byte) (interpreter *Interpreter, err error) {
	var ok bool
	if _, data, ok = expect(data, []byte("WBC\u0000")); !ok {
		return &Interpreter{}, errors.New("Missing magic bytes")
	}

	expectedVersion := []byte{0, 0, 0, 0}
	_, data, ok = expect(data, expectedVersion)
	if !ok {
		return &Interpreter{}, errors.New("Missing or incorrect version number")
	}

	interpreter = &Interpreter{}

	interpreter.Stack = make([]byte, 0, 1024)
	interpreter.callStack = make([]callFrame, 0, 128)

	for len(data) > 0 {
		var header []byte
		header, data, ok = readBytes(data, 4)

		if !ok {
			return &Interpreter{}, errors.New("Missing section header")
		}

		if string(header) == "code" {
			var instructions []Instruction
			instructions, data, err = parseCodeSection(data)

			if err != nil {
				return &Interpreter{}, err
			}

			interpreter.instructions = append(interpreter.instructions, instructions...)
		} else if string(header) == "data" {
			var loadedData []byte
			loadedData, data, err = parseDataSection(data)

			if err != nil {
				return &Interpreter{}, err
			}

			interpreter.Data = append(interpreter.Data, loadedData...)
		} else {
			return &Interpreter{}, errors.New("Unknown section header")
		}
	}

	return
}

func (i *Interpreter) Execute() int {
	for idx := 0; idx < len(i.instructions); idx++ {
		instruction := i.instructions[idx]
		switch instruction.Code {

		case noop:
			continue

		case push:
			switch instruction.Size {

			case 1:
				i.Stack = append(i.Stack, byte(instruction.Args.(uint8)))

			case 2:
				i.Stack = binary.BigEndian.AppendUint16(i.Stack, instruction.Args.(uint16))

			case 4:
				i.Stack = binary.BigEndian.AppendUint32(i.Stack, instruction.Args.(uint32))

			case 8:
				i.Stack = binary.BigEndian.AppendUint64(i.Stack, instruction.Args.(uint64))
			}

		case aloc:
			alocSize := i.popUnsigned(8)
			if alocSize > 1<<(5*8) {
				panic("cant allocate more than " + strconv.Itoa(1<<(5*8)) + " bytes in one call")
			}

			allocIndex := i.nextAllocIdx()

			i.allocations[allocIndex] = make([]byte, alocSize)

			p := pointer{locAlloc, allocIndex, 0}
			i.pushUnsigned(p.toUint64(), 8)

		case stor:
			val := i.popUnsigned(instruction.Size)
			pointer := pointerFromUint64(i.popUnsigned(8))
			destination := i.dataStart(pointer)

			for i := int(instruction.Size) - 1; i >= 0; i-- {
				destination[i] = byte(val)
				val >>= 8
			}

		case load:
			var val uint64
			pointer := pointerFromUint64(i.popUnsigned(8))
			source := i.dataStart(pointer)

			for i := 0; i < int(instruction.Size); i++ {
				val = (val << 8) | uint64(source[i])
			}

			i.pushUnsigned(val, instruction.Size)

		case adds:
			b := i.popSigned(instruction.Size)
			a := i.popSigned(instruction.Size)
			i.pushSigned(a+b, instruction.Size)

		case addu:
			b := i.popUnsigned(instruction.Size)
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(a+b, instruction.Size)

		case addf:
			if instruction.Size == 4 {
				b := math.Float32frombits(uint32(i.popUnsigned(4)))
				a := math.Float32frombits(uint32(i.popUnsigned(4)))
				i.pushUnsigned(uint64(math.Float32bits(a+b)), 4)
			} else {
				b := math.Float64frombits(uint64(i.popUnsigned(8)))
				a := math.Float64frombits(uint64(i.popUnsigned(8)))
				i.pushUnsigned(uint64(math.Float64bits(a+b)), 8)
			}

		case subs, cmps:
			b := i.popSigned(instruction.Size)
			a := i.popSigned(instruction.Size)
			i.pushSigned(a-b, instruction.Size)

		case subu, cmpu:
			b := i.popUnsigned(instruction.Size)
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(a-b, instruction.Size)

		case subf, cmpf:
			if instruction.Size == 4 {
				b := math.Float32frombits(uint32(i.popUnsigned(4)))
				a := math.Float32frombits(uint32(i.popUnsigned(4)))
				i.pushUnsigned(uint64(math.Float32bits(a-b)), 4)
			} else {
				b := math.Float64frombits(uint64(i.popUnsigned(8)))
				a := math.Float64frombits(uint64(i.popUnsigned(8)))
				i.pushUnsigned(uint64(math.Float64bits(a-b)), 8)
			}

		case muls:
			b := i.popSigned(instruction.Size)
			a := i.popSigned(instruction.Size)
			i.pushSigned(a*b, instruction.Size)

		case mulu:
			b := i.popUnsigned(instruction.Size)
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(a*b, instruction.Size)

		case mulf:
			if instruction.Size == 4 {
				b := math.Float32frombits(uint32(i.popUnsigned(4)))
				a := math.Float32frombits(uint32(i.popUnsigned(4)))
				i.pushUnsigned(uint64(math.Float32bits(a*b)), 4)
			} else {
				b := math.Float64frombits(uint64(i.popUnsigned(8)))
				a := math.Float64frombits(uint64(i.popUnsigned(8)))
				i.pushUnsigned(uint64(math.Float64bits(a*b)), 8)
			}

		case divs:
			b := i.popSigned(instruction.Size)
			a := i.popSigned(instruction.Size)
			i.pushSigned(a/b, instruction.Size)

		case divu:
			b := i.popUnsigned(instruction.Size)
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(a/b, instruction.Size)

		case divf:
			if instruction.Size == 4 {
				b := math.Float32frombits(uint32(i.popUnsigned(4)))
				a := math.Float32frombits(uint32(i.popUnsigned(4)))
				i.pushUnsigned(uint64(math.Float32bits(a/b)), 4)
			} else {
				b := math.Float64frombits(uint64(i.popUnsigned(8)))
				a := math.Float64frombits(uint64(i.popUnsigned(8)))
				i.pushUnsigned(uint64(math.Float64bits(a/b)), 8)
			}

		case mods:
			b := i.popSigned(instruction.Size)
			a := i.popSigned(instruction.Size)
			i.pushSigned(a%b, instruction.Size)

		case modu:
			b := i.popUnsigned(instruction.Size)
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(a%b, instruction.Size)

		case dupe:
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(a, instruction.Size)
			i.pushUnsigned(a, instruction.Size)

		case pops:
			i.popUnsigned(instruction.Size)

		case jump:
			idx = int(instruction.Args.(uint32)) - 1

		case jpgt:
			value := i.popSigned(instruction.Size)
			if value > 0 {
				idx = int(instruction.Args.(uint32)) - 1
			}

		case jpge:
			value := i.popSigned(instruction.Size)
			if value >= 0 {
				idx = int(instruction.Args.(uint32)) - 1
			}

		case jpeq:
			value := i.popSigned(instruction.Size)
			if value == 0 {
				idx = int(instruction.Args.(uint32)) - 1
			}

		case jpne:
			value := i.popSigned(instruction.Size)
			if value != 0 {
				idx = int(instruction.Args.(uint32)) - 1
			}

		case jple:
			value := i.popSigned(instruction.Size)
			if value <= 0 {
				idx = int(instruction.Args.(uint32)) - 1
			}

		case jplt:
			value := i.popSigned(instruction.Size)
			if value < 0 {
				idx = int(instruction.Args.(uint32)) - 1
			}

		case prti:
			fmt.Println(i.popSigned(instruction.Size))

		case prtu:
			fmt.Println(i.popUnsigned(instruction.Size))

		case prtf:
			switch instruction.Size {
			case 4:
				fmt.Println(math.Float32frombits(uint32(i.popUnsigned(instruction.Size))))

			case 8:
				fmt.Println(math.Float64frombits(i.popUnsigned(instruction.Size)))
			}

		case prts:
			ptr := pointerFromUint64(i.popUnsigned(8))
			str := i.dataStart(ptr)
			strLen := binary.BigEndian.Uint64(str)
			str = str[8 : 8+strLen]
			fmt.Println(string(str))

		case isgt:
			value := i.popSigned(instruction.Size)
			if value > 0 {
				i.pushUnsigned(1, instruction.Size)
			} else {
				i.pushUnsigned(0, instruction.Size)
			}

		case isge:
			value := i.popSigned(instruction.Size)
			if value >= 0 {
				i.pushUnsigned(1, instruction.Size)
			} else {
				i.pushUnsigned(0, instruction.Size)
			}

		case iseq:
			value := i.popSigned(instruction.Size)
			if value == 0 {
				i.pushUnsigned(1, instruction.Size)
			} else {
				i.pushUnsigned(0, instruction.Size)
			}

		case isne:
			value := i.popSigned(instruction.Size)
			if value != 0 {
				i.pushUnsigned(1, instruction.Size)
			} else {
				i.pushUnsigned(0, instruction.Size)
			}

		case isle:
			value := i.popSigned(instruction.Size)
			if value <= 0 {
				i.pushUnsigned(1, instruction.Size)
			} else {
				i.pushUnsigned(0, instruction.Size)
			}

		case islt:
			value := i.popSigned(instruction.Size)
			if value < 0 {
				i.pushUnsigned(1, instruction.Size)
			} else {
				i.pushUnsigned(0, instruction.Size)
			}

		case over:
			a := i.popUnsigned(instruction.Size)
			b := i.popUnsigned(instruction.Size)

			i.pushUnsigned(b, instruction.Size)
			i.pushUnsigned(a, instruction.Size)
			i.pushUnsigned(b, instruction.Size)

		case swap:
			a := i.popUnsigned(instruction.Size)
			b := i.popUnsigned(instruction.Size)

			i.pushUnsigned(a, instruction.Size)
			i.pushUnsigned(b, instruction.Size)

		case rote:
			a := i.popUnsigned(instruction.Size)
			b := i.popUnsigned(instruction.Size)
			c := i.popUnsigned(instruction.Size)

			i.pushUnsigned(b, instruction.Size)
			i.pushUnsigned(a, instruction.Size)
			i.pushUnsigned(c, instruction.Size)

		case sgne:
			i.pushSigned(i.popSigned(instruction.Size/2), instruction.Size)

		case band:
			b := i.popUnsigned(instruction.Size)
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(a&b, instruction.Size)

		case borr:
			b := i.popUnsigned(instruction.Size)
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(a|b, instruction.Size)

		case bnot:
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(^a, instruction.Size)

		case bxor:
			b := i.popUnsigned(instruction.Size)
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(a^b, instruction.Size)

		case bshl:
			b := i.popUnsigned(instruction.Size)
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(a<<b, instruction.Size)

		case bsrs:
			b := i.popSigned(instruction.Size)
			a := i.popSigned(instruction.Size)
			i.pushSigned(a>>b, instruction.Size)

		case bsru:
			b := i.popUnsigned(instruction.Size)
			a := i.popUnsigned(instruction.Size)
			i.pushUnsigned(a>>b, instruction.Size)

		case call:
			var base pointer
			if i.fargBase != nil {
				base = *i.fargBase
				i.fargBase = nil
			} else {
				base = pointer{locStack, 0, uint64(len(i.Stack))}
			}

			i.callStack = append(i.callStack, callFrame{uint32(idx), base})
			idx = int(instruction.Args.(uint32)) - 1

		case rett:
			frame := i.callStack[len(i.callStack)-1]
			idx = int(frame.instruction)
			i.callStack = i.callStack[:len(i.callStack)-1]
			i.Stack = i.Stack[:frame.basePointer.byteOffset]

		case exit:
			return int(i.popUnsigned(instruction.Size))

		case negs:
			a := i.popSigned(instruction.Size)
			i.pushSigned(-a, instruction.Size)

		case negf:
			if instruction.Size == 4 {
				a := math.Float32frombits(uint32(i.popUnsigned(4)))
				i.pushUnsigned(uint64(math.Float32bits(-a)), 4)
			} else {
				a := math.Float64frombits(uint64(i.popUnsigned(8)))
				i.pushUnsigned(math.Float64bits(-a), 8)
			}

		case base:
			if len(i.callStack) == 0 {
				i.pushUnsigned(pointer{locStack, 0, 0}.toUint64(), 8)
			} else {
				i.pushUnsigned(i.callStack[len(i.callStack)-1].basePointer.toUint64(), 8)
			}

		case free:
			ptr := pointerFromUint64(i.popUnsigned(8))
			if ptr.location != locAlloc {
				panic("cannot free pointer: not dynamically allocated")
			}

			i.allocations[ptr.index] = nil
			i.freeIndexes = &linkedList{ptr.index, i.freeIndexes}

		case farg:
			i.fargBase = &pointer{locStack, 0, uint64(len(i.Stack))}

		case sptr:
			i.pushUnsigned(pointer{locStack, 0, uint64(len(i.Stack))}.toUint64(), 8)

		case cvtf:
			if instruction.Size == 4 {
				a := math.Float64frombits(i.popUnsigned(8))
				i.pushUnsigned(uint64(math.Float32bits(float32(a))), 4)
			} else {
				a := math.Float32frombits(uint32(i.popUnsigned(4)))
				i.pushUnsigned(math.Float64bits(float64(a)), 8)
			}

		case debg:
			fmt.Println("*`debg` instruction read stopping execution*")
			fmt.Println("Stack:")
			fmt.Println(i.Stack)
			fmt.Println("callstack:")
			fmt.Println(i.callStack)
			fmt.Println("data:")
			fmt.Println(i.Data)

			return 1

		default:
			fmt.Println("unknown: ", instruction.Code)
		}
	}

	return 0
}

func (i *Interpreter) popSigned(size int8) int64 {
	var output int64
	switch size {
	case 1:
		output = int64(int8(i.Stack[len(i.Stack)-1]))
	case 2:
		output = int64(int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])))
	case 4:
		output = int64(int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])))
	case 8:
		output = int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:]))
	}

	i.Stack = i.Stack[:len(i.Stack)-int(size)]

	return output
}

func (i *Interpreter) popUnsigned(size int8) uint64 {
	var output uint64
	switch size {
	case 1:
		output = uint64(i.Stack[len(i.Stack)-1])
	case 2:
		output = uint64(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:]))
	case 4:
		output = uint64(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:]))
	case 8:
		output = binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])
	}

	i.Stack = i.Stack[:len(i.Stack)-int(size)]

	return output
}

func (i *Interpreter) pushSigned(value int64, size int8) {
	switch size {
	case 1:
		i.Stack = append(i.Stack, byte(int8(value)))
	case 2:
		i.Stack = binary.BigEndian.AppendUint16(i.Stack, uint16(int16(value)))
	case 4:
		i.Stack = binary.BigEndian.AppendUint32(i.Stack, uint32(int32(value)))
	case 8:
		i.Stack = binary.BigEndian.AppendUint64(i.Stack, uint64(value))
	}
}

func (i *Interpreter) pushUnsigned(value uint64, size int8) {
	switch size {
	case 1:
		i.Stack = append(i.Stack, byte(value))
	case 2:
		i.Stack = binary.BigEndian.AppendUint16(i.Stack, uint16(value))
	case 4:
		i.Stack = binary.BigEndian.AppendUint32(i.Stack, uint32(value))
	case 8:
		i.Stack = binary.BigEndian.AppendUint64(i.Stack, value)
	}
}

func (i *Interpreter) nextAllocIdx() uint16 {
	if i.freeIndexes != nil {
		idx := i.freeIndexes.i
		i.freeIndexes = i.freeIndexes.next
		return idx
	}

	if len(i.allocations) >= math.MaxUint16 {
		panic("No more than " + strconv.Itoa(int(math.MaxUint16)) + " dynamic allocations allowed")
	}

	n := len(i.allocations)

	i.allocations = append(i.allocations, nil)

	return uint16(n)
}

func (i *Interpreter) dataStart(p pointer) []byte {
	switch p.location {
	case locDataSection:
		return i.Data[p.byteOffset:]
	case locStack:
		return i.Stack[p.byteOffset:]
	case locAlloc:
		return i.allocations[p.index][p.byteOffset:]
	default:
		panic("unreachable")
	}
}

func parseCodeSection(data []byte) (instructions []Instruction, choppedData []byte, err error) {
	i, data, ok := readUint32(data)

	if !ok {
		return nil, data, errors.New("Unexpected end of input reading Code section length")
	}

	instructionBytes, choppedData, ok := readBytes(data, i)

	if !ok {
		return nil, choppedData, errors.New("Unexpected end of input reading Code section")
	}

	instructions = make([]Instruction, 0)

	for i := 0; i < len(instructionBytes); {
		opcode := Opcode(instructionBytes[i])
		i++
		size := int8(instructionBytes[i])
		i++

		var args interface{}

		switch opcode {
		case push:
			if len(instructionBytes)-i < int(size) {
				fmt.Println(i)
				fmt.Println(opcode)
				return nil, data, errors.New("Missing argument for opperation")
			}

			switch size {
			case 1:
				args = uint8(data[i])
			case 2:
				args, _, _ = readUint16(instructionBytes[i:])
			case 4:
				args, _, _ = readUint32(instructionBytes[i:])
			case 8:
				args, _, _ = readUint64(instructionBytes[i:])
			default:
				fmt.Println(i)
				panic("unreachable")
			}

			i += int(size)

		case jump, jpgt, jpge, jpeq, jpne, jple, jplt, call:
			if len(instructionBytes)-i < 4 {
				fmt.Println(i)
				fmt.Println(opcode)
				return nil, data, errors.New("Missing argument for opperation")
			}

			args, _, _ = readUint32(data[i:])
			i += 4
		}

		instructions = append(instructions, Instruction{
			Code: opcode,
			Size: size,
			Args: args,
		})
	}

	return
}

func parseDataSection(data []byte) (dataValues []byte, choppedData []byte, err error) {
	i, data, ok := readUint32(data)

	if !ok {
		return nil, data, errors.New("Unexpected end of input reading Data section length")
	}

	dataValues, choppedData, ok = readBytes(data, i)

	if !ok {
		return nil, choppedData, errors.New("Unexpected end of input reading Data section")
	}

	return
}

// TODO: mix of bool and error

func expect(data []byte, expected []byte) (read, chopedData []byte, ok bool) {
	read, chopedData, ok = readBytes(data, uint32(len(expected)))

	if !ok {
		return
	}

	for i := range read {
		if read[i] != expected[i] {
			return nil, data, false
		}
	}

	return
}

func readUint16(data []byte) (i uint16, chopedData []byte, ok bool) {
	if len(data) < 2 {
		return 0, data, false
	}

	i = uint16(data[0])<<(1*8) |
		uint16(data[1])<<(0*8)

	return i, data[2:], true
}

func readUint32(data []byte) (i uint32, chopedData []byte, ok bool) {
	if len(data) < 4 {
		return 0, data, false
	}

	i = uint32(data[0])<<(3*8) |
		uint32(data[1])<<(2*8) |
		uint32(data[2])<<(1*8) |
		uint32(data[3])<<(0*8)

	return i, data[4:], true
}

func readUint64(data []byte) (i uint64, chopedData []byte, ok bool) {
	if len(data) < 8 {
		return 0, data, false
	}

	i = uint64(data[0])<<(7*8) |
		uint64(data[1])<<(6*8) |
		uint64(data[2])<<(5*8) |
		uint64(data[3])<<(4*8) |
		uint64(data[4])<<(3*8) |
		uint64(data[5])<<(2*8) |
		uint64(data[6])<<(1*8) |
		uint64(data[7])<<(0*8)

	return i, data[8:], true
}

func readBytes(data []byte, countBytes uint32) (result []byte, chopedData []byte, ok bool) {
	if len(data) < int(countBytes) {
		return nil, data, false
	}

	return data[:countBytes], data[countBytes:], true
}
