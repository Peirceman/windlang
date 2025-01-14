package main

import (
	"fmt"
	"math"
	"os"
	"reflect"
	"strconv"
	"strings"

	floatparsing "github.com/Peirceman/windlang/floatParsing"
)

func main() {
	fileName := os.Args[1]

	if fileName[len(fileName)-3:] == ".wi" {

		par := ParserFromFilename(fileName)
		ast := par.ParseAll()

		// PPrintln(ast)

		out, err := os.Create("out.wbc")

		if err != nil {
			panic(err)
		}

		defer out.Close()

		err = GenerateBytecode(out, ast)

		if err != nil {
			panic(err)
		}

	} else if fileName[len(fileName)-4:] == ".wbc" {
		file, err := os.Open(fileName)

		if err != nil {
			panic(err)
		}

		defer file.Close()

		interpreter, err := InterpeterFromReader(file)

		if err != nil {
			panic(err)
		}

		interpreter.Execute()

		if len(interpreter.Stack) != 0 {
			fmt.Fprintln(os.Stderr, "ERROR: data left on the stack")
			fmt.Fprintln(os.Stderr, interpreter.Stack)
		}
	} else if fileName == "parse" {
		val := floatparsing.Parse(os.Args[2])
		fmt.Println("value:", val)
		fmt.Printf("bits: %b\n", math.Float64bits(val))

		fmt.Println("\nstdlib:")
		stdVal, _ := strconv.ParseFloat(os.Args[2], 64)
		fmt.Println("value: ", stdVal)
		fmt.Printf("bits: %b\n", math.Float64bits(stdVal))

		fmt.Printf("diff: %b\n", math.Float64bits(stdVal) ^ math.Float64bits(val))
	} else {
		panic("unknown file extention")
	}
}

func PPrintln(a any) {
	pPrint(reflect.ValueOf(a), 0)
	fmt.Println()
}

func pPrint(val reflect.Value, indent int) {
	if val.Type().Name() == "BinaryOp" {
		fmt.Print(val.MethodByName("String").Call(nil))
		return
	}

	if val.IsZero() {
		fmt.Print("<not set>")
		return
	}

	var (
		ind  = strings.Repeat(" ", indent)
		ind2 = strings.Repeat(" ", indent+2)
		typ  = val.Type()
	)

	switch typ.Kind() {
	case reflect.Struct:
		fmt.Printf("%s{\n", typ.Name())
		for i := 0; i < typ.NumField(); i++ {
			fmt.Printf("%s%s: ", ind2, typ.Field(i).Name)
			pPrint(val.Field(i), indent+2)
			fmt.Print(",\n")
		}
		fmt.Printf("%s}", ind)

	case reflect.Pointer:
		fmt.Print("&")
		pPrint(val.Elem(), indent)

	case reflect.Interface:
		pPrint(val.Elem(), indent)

	case reflect.Bool:
		fmt.Print(val.Bool())

	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		fmt.Print(val.Int())

	case reflect.Uint, reflect.Uint8, reflect.Uint16,
		reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		fmt.Print(val.Uint())

	case reflect.Float32, reflect.Float64:
		fmt.Print(val.Float())

	case reflect.Complex64, reflect.Complex128:
		fmt.Print(val.Complex())

	case reflect.Map:
		fmt.Println("map[")
		iter := val.MapRange()
		for iter.Next() {
			fmt.Print(ind2)
			pPrint(iter.Key(), indent+2)
			fmt.Print(": ")
			pPrint(iter.Value(), indent+2)
			fmt.Print(",\n")
		}
		fmt.Printf("%s]", ind)

	case reflect.Slice, reflect.Array:
		const newlineLength = 5
		length := val.Len()
		fmt.Printf("%s{", typ.Name())

		if length >= newlineLength {
			fmt.Printf("\n%s", ind2)
		}

		for i := 0; i < length-1; i++ {
			pPrint(val.Index(i), indent)
			if length < newlineLength {
				fmt.Print(", ")
			} else {
				fmt.Printf(",\n%s", ind2)
			}
		}

		if length >= 1 {
			pPrint(val.Index(length-1), indent)
		}

		if length >= newlineLength {
			fmt.Printf(",\n%s", ind)
		}

		fmt.Print("}")

	case reflect.String:
		fmt.Print(val.String())

	case reflect.UnsafePointer, reflect.Func, reflect.Chan:
		fmt.Print(val.UnsafePointer())
	}
}
