package main

import (
	"fmt"
	"strconv"
)

type Parser struct {
	lex          *Lexer
	currentScope []Scope
	typeDefs     []Type
}

func ParserFromFilename(filename string) (p *Parser) {
	p = &Parser{
		lex: LexerFromFilename(filename),
		currentScope: []Scope{
			{make(VarScope), make(FuncScope)},
		},
	}

	p.addBuiltIns()

	return
}

func ParserFromString(str string) (p *Parser) {
	p = &Parser{
		lex: LexerFromString(str),
		currentScope: []Scope{
			{make(VarScope), make(FuncScope)},
		},
	}
	p.addBuiltIns()

	return
}

func (p *Parser) addBuiltIns() {
	p.typeDefs = []Type{
		SimpleType{KindInt, 1, "int8"},
		SimpleType{KindInt, 2, "int16"},
		SimpleType{KindInt, 4, "int32"},
		SimpleType{KindInt, 8, "int64"},
		SimpleType{KindUint, 1, "uint8"},
		SimpleType{KindUint, 2, "uint16"},
		SimpleType{KindUint, 4, "uint32"},
		SimpleType{KindUint, 8, "uint64"},
		SimpleType{KindFloat, 4, "float32"},
		SimpleType{KindFloat, 8, "float64"},
		SimpleType{KindBool, 4, "bool"},
		PointerType{"string", SimpleType{KindUint, 1, "uint8"}},
	}

	p.addFunc(Func{
		"println",
		[]Var{{"any", SimpleType{KindAny, 0, "any"}, false}},
		SimpleType{},
	})

	p.addFunc(Func{
		"print",
		[]Var{{"any", SimpleType{KindAny, 0, "any"}, false}},
		SimpleType{},
	})
}

func (p *Parser) get(iden Identifier) any {
	for i := len(p.currentScope) - 1; i >= 0; i-- {
		if p.currentScope[i].Contains(iden) {
			return p.currentScope[i].Get(iden)
		}
	}

	return nil
}

func (p *Parser) exists(iden Identifier) bool {
	for i := len(p.currentScope) - 1; i >= 0; i-- {
		if p.currentScope[i].Contains(iden) {
			return true
		}
	}

	return false
}

func (p *Parser) defined(iden Identifier) bool {
	return p.currentScope[len(p.currentScope)-1].Contains(iden)
}

func (p *Parser) addVar(v Var) {
	p.currentScope[len(p.currentScope)-1].AddVar(v)
}

func (p *Parser) addFunc(f Func) {
	p.currentScope[len(p.currentScope)-1].AddFunc(f)
}
func (p *Parser) ParseAll() CodeBlockNode {
	result := CodeBlockNode{
		Statements: make([]AstNode, 0),
	}

	for statement, eof := p.ParseTopLevel(); !eof; statement, eof = p.ParseTopLevel() {
		if statement != nil {
			result.Statements = append(result.Statements, statement)
		}
	}

	result.scope = p.currentScope[0]

	return result
}

func (p *Parser) ParseTopLevel() (AstNode, bool) {
	if TTCount != 61 {
		panic("TokenType enum length changed: " + strconv.Itoa(int(TTCount)))
	}

	tok := p.lex.PeekToken()
	switch tok.typ {
	case TTConst:
		p.lex.NextToken()
		name := p.expect(TTIdentifier)
		p.expect(TTColon)
		typ := p.parseType()

		if tok := p.lex.PeekToken(); tok.typ != TTAssign {
			p.expect(TTSemiColon)

			if p.defined(Identifier(name.literal)) {
				panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
			}

			node := VarNode{
				Var: Var{
					name:    Identifier(name.literal),
					typ:     typ,
					isConst: true,
				},
				Value: nil,
			}

			p.addVar(node.Var)

			return node, false
		}

		p.expect(TTAssign)

		node := VarNode{
			Var: Var{
				name:    Identifier(name.literal),
				typ:     typ,
				isConst: true,
			},
			Value: p.parseExpression(),
		}

		if !EqualTypes(node.typ, node.Value.returnType()) {
			panic(p.lex.curLoc.String() + " lhs and rhs types dont match")
		}

		p.expect(TTSemiColon)

		if p.defined(Identifier(name.literal)) {
			panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
		}

		p.addVar(node.Var)

		return node, false

	case TTVar:
		p.lex.NextToken()
		name := p.expect(TTIdentifier)
		p.expect(TTColon)
		typ := p.parseType()

		if tok := p.lex.PeekToken(); tok.typ != TTAssign {
			p.expect(TTSemiColon)
			if p.defined(Identifier(name.literal)) {
				panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
			}

			node := VarNode{
				Var:   Var{name: Identifier(name.literal), typ: typ, isConst: false},
				Value: nil,
			}

			p.addVar(node.Var)

			return node, false
		}

		p.expect(TTAssign)

		node := VarNode{
			Var:   Var{name: Identifier(name.literal), typ: typ, isConst: false},
			Value: p.parseExpression(),
		}

		p.expect(TTSemiColon)

		if !EqualTypes(node.typ, node.Value.returnType()) {
			fmt.Println(node.typ)
			fmt.Println(node.Value.returnType())
			panic(p.lex.curLoc.String() + " lhs and rhs types dont match")
		}

		if p.defined(Identifier(name.literal)) {
			panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
		}

		p.addVar(node.Var)

		return node, false

	case TTFn:
		return p.parseFunc()

	case TTType:
		p.lex.NextToken()

		name := p.expect(TTIdentifier)

		typ := p.parseType()
		typ = typ.SetName(Identifier(name.literal))

		p.typeDefs = append(p.typeDefs, typ)

		p.expect(TTSemiColon)

		return nil, false

	case TTEOF:
		return nil, true
	}

	panic(tok.loc.String() + " syntax error") // TODO: better error handling
}

func (p *Parser) parseFunctionBody() (AstNode, bool) {
	if TTCount != 61 {
		panic("TokenType enum length changed: " + strconv.Itoa(int(TTCount)))
	}

	tok := p.lex.PeekToken()
	switch tok.typ {
	case TTConst:
		p.lex.NextToken()
		name := p.expect(TTIdentifier)
		p.expect(TTColon)
		typ := p.parseType()

		if tok := p.lex.PeekToken(); tok.typ != TTAssign {
			p.expect(TTSemiColon)
			if p.defined(Identifier(name.literal)) {
				panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
			}

			node := VarNode{
				Var: Var{
					name:    Identifier(name.literal),
					typ:     typ,
					isConst: true,
				},
				Value: nil,
			}

			p.addVar(node.Var)

			return node, false
		}

		p.expect(TTAssign)

		node := VarNode{
			Var: Var{
				name:    Identifier(name.literal),
				typ:     typ,
				isConst: true,
			},
			Value: p.parseExpression(),
		}

		p.expect(TTSemiColon)

		if !EqualTypes(node.typ, node.Value.returnType()) {
			panic(fmt.Sprintf(p.lex.curLoc.String()+" lhs and rhs types dont match: %v, %v", node.typ, node.Value.returnType()))
		}

		if p.defined(Identifier(name.literal)) {
			panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
		}

		p.addVar(node.Var)
		return node, false

	case TTVar:
		p.lex.NextToken()
		name := p.expect(TTIdentifier)
		p.expect(TTColon)
		typ := p.parseType()

		if tok := p.lex.PeekToken(); tok.typ != TTAssign {
			p.expect(TTSemiColon)
			if p.defined(Identifier(name.literal)) {
				panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
			}

			node := VarNode{
				Var: Var{
					name:    Identifier(name.literal),
					typ:     typ,
					isConst: false,
				},
				Value: nil,
			}

			p.addVar(node.Var)

			return node, false
		}

		p.expect(TTAssign)

		node := VarNode{
			Var: Var{
				name:    Identifier(name.literal),
				typ:     typ,
				isConst: false,
			},
			Value: p.parseExpression(),
		}

		p.expect(TTSemiColon)

		if !EqualTypes(node.typ, node.Value.returnType()) {
			panic(p.lex.curLoc.String() + " lhs and rhs types dont match")
		}

		if p.defined(Identifier(name.literal)) {
			panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
		}

		p.addVar(node.Var)
		return node, false

	case TTFn:
		return p.parseFunc()

	case TTReturn:
		p.lex.NextToken()
		tok := p.lex.PeekToken()

		if tok.typ == TTSemiColon {
			p.lex.nextToken()

			return ReturnNode{nil}, false
		}

		node := ReturnNode{p.parseExpression()}
		p.expect(TTSemiColon)

		return node, false

	case TTLSquirly:
		p.lex.NextToken()
		return p.parseCodeBlock()

	case TTIdentifier, TTStar:
		node := ExpressionNode{p.parseExpression()}

		p.expect(TTSemiColon)

		return node, false

	case TTSemiColon: // skip token
		p.lex.NextToken()
		return nil, false

	case TTIf:
		p.lex.NextToken()
		node := IfChain{}
		node.IfCondition = p.parseExpression()

		if node.IfCondition.returnType().Kind() != KindBool { // Kind bool instead of "real" bool because typedefed bools can also be used
			panic(p.lex.curLoc.String() + " boolean expression expected")
		}

		p.expectPeek(TTLSquirly)

		statements, eof := p.parseFunctionBody()
		if eof {
			panic("unreachable")
		}

		node.IfStatement = statements.(CodeBlockNode)

		for nextToken := p.lex.PeekToken(); nextToken.typ == TTElif; nextToken = p.lex.PeekToken() {
			p.lex.NextToken()

			condition := p.parseExpression()

			if condition.returnType().Kind() != KindBool { // same as before
				panic(p.lex.curLoc.String() + " boolean expression expected")
			}

			node.ElifConditions = append(node.ElifConditions, condition)

			p.expectPeek(TTLSquirly)

			statements, eof := p.parseFunctionBody()

			if eof {
				panic("unreachable")
			}

			node.ElifStatements = append(node.ElifStatements, statements.(CodeBlockNode))
		}

		if nextToken := p.lex.PeekToken(); nextToken != nil && nextToken.typ == TTElse {
			node.hasElse = true
			p.lex.NextToken()

			p.expectPeek(TTLSquirly)

			statements, eof := p.parseFunctionBody()
			if eof {
				panic("unreachable")
			}

			node.ElseStatement = statements.(CodeBlockNode)
		}

		return node, false

	case TTWhile:
		p.lex.NextToken()
		node := WhileNode{}
		node.Condition = p.parseExpression()

		if node.Condition.returnType().Kind() != KindBool { // same as in if
			panic(p.lex.curLoc.String() + " boolean expression expected")
		}

		p.expectPeek(TTLSquirly)

		statements, eof := p.parseFunctionBody()

		if eof {
			panic("unreachable")
		}

		node.Loop = statements.(CodeBlockNode)

		return node, false

	case TTEOF:
		return nil, true
	}

	panic(tok.String() + " syntax error ") // TODO: better error handling
}

func (p *Parser) parseCodeBlock() (CodeBlockNode, bool) {
	block := CodeBlockNode{make([]AstNode, 0), Scope{make(VarScope), make(FuncScope)}}
	p.currentScope = append(p.currentScope, block.scope)

	for tok := p.lex.PeekToken(); tok.typ != TTEOF && tok.typ != TTRSquirly; tok = p.lex.PeekToken() {
		statement, eof := p.parseFunctionBody()
		if eof {
			panic(tok.loc.String() + " `}` excpected")
		}

		if statement == nil {
			continue
		}

		block.Statements = append(block.Statements, statement)
	}

	block.Statements = block.Statements[:len(block.Statements):len(block.Statements)]
	p.expect(TTRSquirly)

	p.currentScope = p.currentScope[:len(p.currentScope)-1]

	return block, false
}

func (p *Parser) parseFunc() (FuncNode, bool) {
	p.lex.NextToken()
	node := FuncNode{returnType: TypeVoid}

	name := p.expect(TTIdentifier)
	node.name = Identifier(name.literal)
	if p.defined(node.name) {
		panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
	}

	p.expect(TTLBrace)
	scope := Scope{make(VarScope), make(FuncScope)}

	for tok := p.lex.PeekToken(); tok != nil && tok.typ != TTRBrace; tok = p.lex.PeekToken() {
		arg := Var{}
		arg.name = Identifier(p.expect(TTIdentifier).literal)

		p.expect(TTColon)

		arg.typ = p.parseType()

		if arg.typ.Kind() == KindStruct {
			panic("cannot pass struct as an argument yet")
		}

		node.Args = append(node.Args, arg)
		scope.AddVar(arg)

		tok = p.lex.PeekToken()
		if tok == nil || tok.typ != TTComma {
			break
		}

		p.lex.NextToken()
	}

	p.expect(TTRBrace)

	tok := p.lex.PeekToken()
	if tok.typ == TTColon {
		p.lex.NextToken()
		node.returnType = p.parseType()
		if node.returnType.Kind() == KindStruct {
			panic("func may not return a struct yet")
		}
	}

	p.expect(TTLSquirly)

	p.currentScope = append(p.currentScope, scope)

	body, eof := p.parseCodeBlock()
	node.Body = body

	p.currentScope = p.currentScope[:len(p.currentScope)-1]

	p.addFunc(Func{node.name, node.Args, node.returnType})

	return node, eof
}

func (p *Parser) parseType() Type {
	tok := p.lex.NextToken()

	switch tok.typ {
	case TTIdentifier:
		for _, typedef := range p.typeDefs {
			if typedef.Name() == Identifier(tok.literal) {
				return typedef
			}
		}

	case TTLSquare:
		panic("arrays not yet fully implemented")
		p.expect(TTRSquare)
		_ = p.parseType() // inner

		// return SimpleType{KindArray, 8, "", &inner}

	case TTAmp:
		inner := p.parseType()
		return PointerType{"", inner}

	case TTAnd: // special case for `&&` which is seen as one token
		inner := p.parseType()
		return PointerType{"", PointerType{"", inner}}

	case TTStruct:
		p.expect(TTLSquirly)

		typ := StructType{}

		for tok := p.lex.PeekToken(); tok.typ != TTEOF && tok.typ != TTRSquirly; tok = p.lex.PeekToken() {
			field := StructField{}

			field.offset = typ.size
			tok := p.expect(TTIdentifier)
			field.name = Identifier(tok.literal)

			p.expect(TTColon)

			field.typ = p.parseType()

			for _, prevField := range typ.fields {
				if prevField.name == field.name {
					panic(tok.loc.String() + "ERROR: field redeclared")
				}
			}

			typ.size += field.typ.Size()

			typ.fields = append(typ.fields, field)

			tok = p.lex.PeekToken()

			if tok.typ != TTComma {
				break
			}

			p.lex.NextToken()
		}

		// round field up to multiple of 8
		typ.size = (typ.size + 7) / 8 * 8

		p.expect(TTRSquirly)

		return typ
	}

	panic(tok.loc.String() + " Error: expected type")
}

func (p *Parser) expect(typ TokenType) *Token {
	tok := p.lex.NextToken()
	if tok.typ != typ {
		panic(tok.loc.String() + " " + tok.literal + " syntax error, expected: " + typ.String()) // TODO: better error handling
	}

	return tok
}

func (p *Parser) expectPeek(typ TokenType) *Token {
	tok := p.lex.PeekToken()
	if tok.typ != typ {
		panic(tok.loc.String() + " " + tok.literal + " syntax error, expected: " + typ.String()) // TODO: better error handling
	}

	return tok
}

func (p *Parser) parseExpression() Expression {
	return p.parseBinary(0)
}

func (p *Parser) parseBinary(precedence int) Expression {
	if precedence > MaxPrecedence {
		return p.parseUnary()
	}

	loc := p.lex.curLoc
	lhs := p.parseBinary(precedence + 1)
	if lhs == nil {
		return nil
	}

	if LeftToRight(precedence) {
		tok := p.lex.PeekToken()
		opp := tok.typ.ToBinOp()
		for tok != nil && tok.typ != TTEOF && opp != -1 && opp.Precedence() == precedence {
			p.lex.NextToken()

			rhs := p.parseBinary(precedence + 1)
			if rhs == nil {
				panic(p.lex.curLoc.String() + " opperand expected") // TODO: better error handling
			}

			if precedence == BOAssign.Precedence() {
				switch lhs := lhs.(type) {
				case Var:
					if !lhs.isConst {
						goto ok1 // oh noooooo goto such unreadableness
					}
				case UnaryOpNode:
					if lhs.Op == UODeref {
						goto ok1
					}
				}

				panic(p.lex.curLoc.String() + " cannot assign to lhs")

			ok1:
			}

			var err error
			lhs, err = NewBinaryOpNode(lhs, rhs, opp)
			if err != nil {
				fmt.Println(opp.String())
				panic(loc.String() + err.Error()) // TODO: better error handling
			}

			tok = p.lex.PeekToken()
			opp = tok.typ.ToBinOp()
		}
	} else {
		tok := p.lex.PeekToken()
		opp := tok.typ.ToBinOp()
		for tok.typ != TTEOF && opp != -1 && opp.Precedence() == precedence {
			p.lex.NextToken()
			rhs := p.parseBinary(precedence)
			if rhs == nil {
				panic(loc.String() + " opperand expected") // TODO: better error handling
			}

			if precedence == BOAssign.Precedence() {
				switch lhs := lhs.(type) {
				case Var:
					if !lhs.isConst {
						goto ok2
					}
				case UnaryOpNode:
					if lhs.Op == UODeref {
						goto ok2
					}
				case StructIndex:
					if _, ok := lhs.base.(Var); ok {
						goto ok2
					}
				}

				panic(p.lex.curLoc.String() + " cannot assign to lhs")

			ok2:
			}

			var err error
			lhs, err = NewBinaryOpNode(lhs, rhs, opp)
			if err != nil {
				fmt.Println(opp.String())
				panic(p.lex.curLoc.String() + err.Error()) // TODO: better error handling
			}

			tok = p.lex.PeekToken()
			opp = tok.typ.ToBinOp()
		}
	}

	return lhs
}

func (p *Parser) parseUnary() Expression {
	tok := p.lex.PeekToken()
	loc := tok.loc
	opp := tok.typ.ToUnOp()

	if opp != -1 && opp.OnLeftSide() {
		p.lex.NextToken()
		expression := p.parseUnary()
		if expression == nil {
			panic(p.lex.curLoc.String() + " Error: opperand expected")
		}

		unOp, err := NewUnaryOpNode(expression, opp)

		if err != nil {
			panic(loc.String() + err.Error())
		}

		return unOp
	}

	expression := p.parsePrimary()

	if expression == nil {
		panic(p.lex.curLoc.String() + " Error: opperand expected")
	}

	tok = p.lex.PeekToken()
	var err error
	for opp = tok.typ.ToUnOp(); opp != -1 && !opp.OnLeftSide(); opp = tok.typ.ToUnOp() {
		p.lex.NextToken()
		expression, err = NewUnaryOpNode(expression, opp)

		if err != nil {
			panic(p.lex.curLoc.String() + err.Error())
		}

		tok = p.lex.PeekToken()
	}

	return expression
}

func (p *Parser) parsePrimary() Expression {
	tok := p.lex.PeekToken()
	if tok.typ == TTEOF {
		panic("opperand expected")
	}

	switch tok.typ {
	case TTIdentifier:
		p.lex.NextToken()
		if !p.exists(Identifier(tok.literal)) {
			panic(tok.loc.String() + " Undefinded name: " + tok.literal)
		}

		var expr Expression

		if next := p.lex.PeekToken(); next.typ == TTLBrace {
			funcName := Identifier(tok.literal)

			funcCall := FuncCall{}

			if val, ok := p.get(funcName).(Func); ok {
				funcCall.fun = val
			} else {
				p.lex.NextToken()
				fmt.Println(val)
				panic(p.lex.curLoc.String() + " Can only call functions")
			}

			p.lex.NextToken() // always`(` because of if condition

			argIdx := -1

			for tok := p.lex.PeekToken(); tok != nil && tok.typ != TTRBrace; tok = p.lex.PeekToken() {
				if len(funcCall.Args) >= len(funcCall.fun.Args) {
					panic(p.lex.curLoc.String() + " Error: to many arguments to function")
				}

				loc := p.lex.curLoc
				arg := p.parseExpression()

				funcCall.Args = append(funcCall.Args, arg)
				argIdx++

				expectedType := funcCall.fun.Args[argIdx].returnType()

				if !EqualTypes(arg.returnType(), expectedType) && expectedType.Kind() != KindAny {
					panic(loc.String() + " Error: argument type mismatch")
				}

				tok = p.lex.PeekToken()
				if tok == nil || tok.typ != TTComma {
					break
				}

				p.lex.NextToken()
			}

			if len(funcCall.Args) < len(funcCall.fun.Args) {
				panic(p.lex.curLoc.String() + " Error: to few arguments to function")
			}

			p.expect(TTRBrace)

			expr = funcCall
		} else {
			expr = p.get(Identifier(tok.literal)).(Var)
		}

		next := p.lex.PeekToken()

		if next.typ != TTPeriod {
			return expr
		}

		curVal := StructIndex{base: expr, typ: expr.returnType(), offset: 0}
		expr = curVal

		for ; next.typ == TTPeriod; next = p.lex.PeekToken() {
			p.lex.NextToken()

			if curVal.typ.Kind() != KindStruct {
				panic(p.lex.curLoc.String() + "ERROR: not a struct")
			}

			next = p.lex.PeekToken()

			if next.typ == TTLSquirly {
				panic("struct initialization")
			}

			if next.typ == TTIdentifier {
				p.lex.NextToken()

				var field StructField

				for _, field = range curVal.typ.(StructType).fields {
					if field.name == Identifier(next.literal) {
						goto fieldOk
					}
				}

				panic(p.lex.curLoc.String() + "ERROR: no field `" + next.literal + "`")

			fieldOk:
				curVal.offset += field.offset
				curVal.typ = field.typ
				expr = curVal
				continue
			}

			p.expect(TTIdentifier) // throw error
		}

		return expr

	case TTInt:
		p.lex.NextToken()
		val := tok.extraInfo.(int)
		return IntLit{val}

	case TTFloat:
		p.lex.NextToken()
		return FloatLit{tok.literal}

	case TTString:
		p.lex.NextToken()
		return StrLit{tok.extraInfo.(string), tok.literal}

	case TTChar:
		p.lex.NextToken()
		return CharLit{tok.extraInfo.(rune), tok.literal}

	case TTTrue:
		p.lex.NextToken()
		return BoolLit{true}

	case TTFalse:
		p.lex.NextToken()
		return BoolLit{false}

	case TTLBrace:
		p.lex.NextToken()
		result := p.parseBinary(0)

		next := p.lex.NextToken().typ
		if next == TTRBrace {
			return result
		}

		if next == TTColon {
			typ := p.parseType()

			// TODO: some sort of `canCast` function
			innerKind := typ.Kind()
			resultKind := result.returnType().Kind()
			sameKind := innerKind == resultKind
			intToUint := (innerKind == KindUint && resultKind == KindInt) ||
				(typ.Kind() == KindInt && resultKind == KindUint)

			if !sameKind && !intToUint {
				panic(fmt.Errorf("%s ERROR: cannot cast: incompatible types", p.lex.curLoc.String()))
			}

			p.expect(TTRBrace)

			return Cast{inner: result, newType: typ}
		}

		p.expect(TTRBrace)
	}

	panic(p.lex.NextToken().String() + " illegal token") // TODO: better error handling
}
