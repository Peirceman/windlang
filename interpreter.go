package main

import (
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"math"
)

type Interpreter struct {
	instructions []Instruction
	Data         map[uint32][]byte
	Stack        []byte
}

func InterpeterFromReader(r io.Reader) (*Interpreter, error) {
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
	interpreter.Data = make(map[uint32][]byte)

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
			var loadedData map[uint32][]byte
			loadedData, data, err = parseDataSection(data)

			if err != nil {
				return &Interpreter{}, err
			}

			for id, value := range loadedData {
				interpreter.Data[id] = value
			}
		} else {
			return &Interpreter{}, errors.New("Unknown section header")
		}
	}

	return
}

func (i *Interpreter) Execute() {
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
		case decl:
			i.Data[instruction.Args.(uint32)] = make([]byte, instruction.Size)
		case popv:
			copy(i.Data[instruction.Args.(uint32)], i.Stack[len(i.Stack)-int(instruction.Size):])
			i.Stack = i.Stack[:len(i.Stack)-int(instruction.Size)]
		case pshv:
			i.Stack = append(i.Stack, i.Data[instruction.Args.(uint32)]...)
		case adds:
			switch instruction.Size {
			case 1:
				a := int8(i.Stack[len(i.Stack)-2]) + int8(i.Stack[len(i.Stack)-1])
				i.Stack[len(i.Stack)-1] = uint8(a)
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				b := int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:]))
				a := int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-4:]))
				i.Stack = binary.BigEndian.AppendUint16(i.Stack[:len(i.Stack)-4], uint16(a+b))
			case 4:
				b := int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:]))
				a := int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:]))
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], uint32(a+b))
			case 8:
				b := int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:]))
				a := int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:]))
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], uint64(a+b))
			}
		case addu:
			switch instruction.Size {
			case 1:
				i.Stack[len(i.Stack)-2] += i.Stack[len(i.Stack)-1]
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				b := binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])
				a := binary.BigEndian.Uint16(i.Stack[len(i.Stack)-4:])
				i.Stack = binary.BigEndian.AppendUint16(i.Stack[:len(i.Stack)-4], a+b)
			case 4:
				b := binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])
				a := binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:])
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], a+b)
			case 8:
				b := binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])
				a := binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:])
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], a+b)
			}
		case addf:
			switch instruction.Size {
			case 4:
				b := math.Float32frombits(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:]))
				a := math.Float32frombits(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:]))
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], math.Float32bits(a+b))
			case 8:
				b := math.Float64frombits(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:]))
				a := math.Float64frombits(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:]))
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], math.Float64bits(a+b))
			}
		case subs, cmps:
			switch instruction.Size {
			case 1:
				a := int8(i.Stack[len(i.Stack)-2]) - int8(i.Stack[len(i.Stack)-1])
				i.Stack[len(i.Stack)-1] = uint8(a)
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				b := int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:]))
				a := int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-4:]))
				i.Stack = binary.BigEndian.AppendUint16(i.Stack[:len(i.Stack)-4], uint16(a-b))
			case 4:
				b := int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:]))
				a := int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:]))
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], uint32(a-b))
			case 8:
				b := int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:]))
				a := int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:]))
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], uint64(a-b))
			}
		case subu, cmpu:
			switch instruction.Size {
			case 1:
				i.Stack[len(i.Stack)-2] -= i.Stack[len(i.Stack)-1]
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				b := binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])
				a := binary.BigEndian.Uint16(i.Stack[len(i.Stack)-4:])
				i.Stack = binary.BigEndian.AppendUint16(i.Stack[:len(i.Stack)-4], a-b)
			case 4:
				b := binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])
				// fmt.Println(idx)
				a := binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:])
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], a-b)
			case 8:
				b := binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])
				a := binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:])
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], a-b)
			}
		case subf, cmpf:
			switch instruction.Size {
			case 4:
				b := math.Float32frombits(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:]))
				a := math.Float32frombits(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:]))
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], math.Float32bits(a-b))
			case 8:
				b := math.Float64frombits(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:]))
				a := math.Float64frombits(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:]))
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], math.Float64bits(a-b))
			}
		case muls:
			switch instruction.Size {
			case 1:
				a := int8(i.Stack[len(i.Stack)-2]) * int8(i.Stack[len(i.Stack)-1])
				i.Stack[len(i.Stack)-1] = uint8(a)
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				b := int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:]))
				a := int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-4:]))
				i.Stack = binary.BigEndian.AppendUint16(i.Stack[:len(i.Stack)-4], uint16(a*b))
			case 4:
				b := int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:]))
				a := int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:]))
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], uint32(a*b))
			case 8:
				b := int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:]))
				a := int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:]))
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], uint64(a*b))
			}
		case mulu:
			switch instruction.Size {
			case 1:
				i.Stack[len(i.Stack)-2] *= i.Stack[len(i.Stack)-1]
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				b := binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])
				a := binary.BigEndian.Uint16(i.Stack[len(i.Stack)-4:])
				i.Stack = binary.BigEndian.AppendUint16(i.Stack[:len(i.Stack)-4], a*b)
			case 4:
				b := binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])
				a := binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:])
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], a*b)
			case 8:
				b := binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])
				a := binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:])
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], a*b)
			}
		case mulf:
			switch instruction.Size {
			case 4:
				b := math.Float32frombits(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:]))
				a := math.Float32frombits(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:]))
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], math.Float32bits(a*b))
			case 8:
				b := math.Float64frombits(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:]))
				a := math.Float64frombits(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:]))
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], math.Float64bits(a*b))
			}
		case divs:
			switch instruction.Size {
			case 1:
				a := int8(i.Stack[len(i.Stack)-2]) / int8(i.Stack[len(i.Stack)-1])
				i.Stack[len(i.Stack)-1] = uint8(a)
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				b := int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:]))
				a := int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-4:]))
				i.Stack = binary.BigEndian.AppendUint16(i.Stack[:len(i.Stack)-4], uint16(a/b))
			case 4:
				b := int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:]))
				a := int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:]))
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], uint32(a/b))
			case 8:
				b := int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:]))
				a := int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:]))
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], uint64(a/b))
			}
		case divu:
			switch instruction.Size {
			case 1:
				i.Stack[len(i.Stack)-2] /= i.Stack[len(i.Stack)-1]
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				b := binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])
				a := binary.BigEndian.Uint16(i.Stack[len(i.Stack)-4:])
				i.Stack = binary.BigEndian.AppendUint16(i.Stack[:len(i.Stack)-4], a/b)
			case 4:
				b := binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])
				a := binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:])
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], a/b)
			case 8:
				b := binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])
				a := binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:])
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], a/b)
			}
		case divf:
			switch instruction.Size {
			case 4:
				b := math.Float32frombits(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:]))
				a := math.Float32frombits(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:]))
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], math.Float32bits(a/b))
			case 8:
				b := math.Float64frombits(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:]))
				a := math.Float64frombits(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:]))
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], math.Float64bits(a/b))
			}
		case mods:
			switch instruction.Size {
			case 1:
				a := int8(i.Stack[len(i.Stack)-2]) % int8(i.Stack[len(i.Stack)-1])
				i.Stack[len(i.Stack)-1] = uint8(a)
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				b := int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:]))
				a := int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-4:]))
				i.Stack = binary.BigEndian.AppendUint16(i.Stack[:len(i.Stack)-4], uint16(a%b))
			case 4:
				b := int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:]))
				a := int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:]))
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], uint32(a%b))
			case 8:
				b := int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:]))
				a := int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:]))
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], uint64(a%b))
			}
		case modu:
			switch instruction.Size {
			case 1:
				i.Stack[len(i.Stack)-2] %= i.Stack[len(i.Stack)-1]
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				b := binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])
				a := binary.BigEndian.Uint16(i.Stack[len(i.Stack)-4:])
				i.Stack = binary.BigEndian.AppendUint16(i.Stack[:len(i.Stack)-4], a%b)
			case 4:
				b := binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])
				a := binary.BigEndian.Uint32(i.Stack[len(i.Stack)-8:])
				i.Stack = binary.BigEndian.AppendUint32(i.Stack[:len(i.Stack)-8], a%b)
			case 8:
				b := binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])
				a := binary.BigEndian.Uint64(i.Stack[len(i.Stack)-16:])
				i.Stack = binary.BigEndian.AppendUint64(i.Stack[:len(i.Stack)-16], a%b)
			}
		case dupe:
			i.Stack = append(i.Stack, i.Stack[len(i.Stack)-int(instruction.Size):]...)
		case pops:
			i.Stack = i.Stack[:len(i.Stack)-int(instruction.Size)]
		case jump:
			idx = int(instruction.Args.(uint32))-1
		case jpgt:
			var shouldJump bool
			switch instruction.Size {
			case 1:
				shouldJump = int8(i.Stack[len(i.Stack)-1]) > 0
			case 2:
				shouldJump = int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])) > 0
			case 4:
				shouldJump = int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])) > 0
			case 8:
				shouldJump = int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])) > 0
			}

			if shouldJump {
				idx = int(instruction.Args.(uint32))-1
			}

			i.Stack = i.Stack[:len(i.Stack)-int(instruction.Size)]
		case jpge:
			var shouldJump bool
			switch instruction.Size {
			case 1:
				shouldJump = int8(i.Stack[len(i.Stack)-1]) >= 0
			case 2:
				shouldJump = int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])) >= 0
			case 4:
				shouldJump = int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])) >= 0
			case 8:
				shouldJump = int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])) >= 0
			}

			if shouldJump {
				idx = int(instruction.Args.(uint32))-1
			}

			i.Stack = i.Stack[:len(i.Stack)-int(instruction.Size)]
		case jpeq:
			var shouldJump bool
			switch instruction.Size {
			case 1:
				shouldJump = int8(i.Stack[len(i.Stack)-1]) == 0
			case 2:
				shouldJump = int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])) == 0
			case 4:
				shouldJump = int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])) == 0
			case 8:
				shouldJump = int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])) == 0
			}

			if shouldJump {
				idx = int(instruction.Args.(uint32))-1
			}

			i.Stack = i.Stack[:len(i.Stack)-int(instruction.Size)]
		case jple:
			var shouldJump bool
			switch instruction.Size {
			case 1:
				shouldJump = int8(i.Stack[len(i.Stack)-1]) <= 0
			case 2:
				shouldJump = int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])) <= 0
			case 4:
				shouldJump = int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])) <= 0
			case 8:
				shouldJump = int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])) <= 0
			}

			if shouldJump {
				idx = int(instruction.Args.(uint32))-1
			}

			i.Stack = i.Stack[:len(i.Stack)-int(instruction.Size)]
		case jplt:
			var shouldJump bool
			switch instruction.Size {
			case 1:
				shouldJump = int8(i.Stack[len(i.Stack)-1]) < 0
			case 2:
				shouldJump = int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])) < 0
			case 4:
				shouldJump = int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])) < 0
			case 8:
				shouldJump = int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])) < 0
			}

			if shouldJump {
				idx = int(instruction.Args.(uint32))-1
			}

			i.Stack = i.Stack[:len(i.Stack)-int(instruction.Size)]
		case prti:
			switch instruction.Size {
			case 1:
				fmt.Println(int8(i.Stack[len(i.Stack)-1]))
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				fmt.Println(int16(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:])))
				i.Stack = i.Stack[:len(i.Stack)-2]
			case 4:
				fmt.Println(int32(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])))
				i.Stack = i.Stack[:len(i.Stack)-4]
			case 8:
				fmt.Println(int64(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])))
				i.Stack = i.Stack[:len(i.Stack)-8]
			}
		case prtu:
			switch instruction.Size {
			case 1:
				fmt.Println(i.Stack[len(i.Stack)-1])
				i.Stack = i.Stack[:len(i.Stack)-1]
			case 2:
				fmt.Println(binary.BigEndian.Uint16(i.Stack[len(i.Stack)-2:]))
				i.Stack = i.Stack[:len(i.Stack)-2]
			case 4:
				fmt.Println(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:]))
				i.Stack = i.Stack[:len(i.Stack)-4]
			case 8:
				fmt.Println(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:]))
				i.Stack = i.Stack[:len(i.Stack)-8]
			}
		case prtf:
			switch instruction.Size {
			case 4:
				fmt.Println(math.Float32frombits(binary.BigEndian.Uint32(i.Stack[len(i.Stack)-4:])))
				i.Stack = i.Stack[:len(i.Stack)-4]
			case 8:
				fmt.Println(math.Float64frombits(binary.BigEndian.Uint64(i.Stack[len(i.Stack)-8:])))
				i.Stack = i.Stack[:len(i.Stack)-8]
			}
		case prts:
			fmt.Println(string(i.Data[instruction.Args.(uint32)]))
		default:
			fmt.Println("unknown: ", instruction.Code)
		}
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
				panic("unreachable")
			}

			i += int(size)

		case popv, pshv, decl, jump, jpgt, jpge, jpeq, jple, jplt, prts:
			if len(instructionBytes)-i < 4 {
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

func parseDataSection(data []byte) (dataValues map[uint32][]byte, choppedData []byte, err error) {
	i, data, ok := readUint32(data)

	if !ok {
		return nil, data, errors.New("Unexpected end of input reading Data section length")
	}

	dataBytes, choppedData, ok := readBytes(data, i)

	if !ok {
		return nil, choppedData, errors.New("Unexpected end of input reading Data section")
	}

	dataValues = make(map[uint32][]byte, 0)

	for i := 0; i < len(dataBytes); {
		if len(dataBytes)-i < 8 {
			return nil, data, errors.New("Unexpected end of input reading data id and length")
		}

		id, _, _ := readUint32(data[i:])
		i += 4
		length, _, _ := readUint32(data[i:])
		i += 4

		if len(dataBytes)-i < int(length) {
			return nil, data, errors.New("Unexpected end of input reading data")
		}

		dataValues[id] = make([]byte, length)
		copy(dataValues[id], dataBytes[i:])
		i += int(length)
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
