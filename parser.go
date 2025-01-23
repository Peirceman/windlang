package main

import (
	"fmt"
	"strconv"
	"windlang/ast"
)

type Parser struct {
	lex          *Lexer
	currentScope []ast.Scope
	typeDefs     []ast.Type
}

func ParserFromFilename(filename string) (p *Parser) {
	p = &Parser{
		lex: LexerFromFilename(filename),
		currentScope: []ast.Scope{
			{Vars: make(ast.VarScope), Funcs: make(ast.FuncScope)},
		},
	}

	p.addBuiltIns()

	return
}

func ParserFromString(str string) (p *Parser) {
	p = &Parser{
		lex: LexerFromString(str),
		currentScope: []ast.Scope{
			{Vars: make(ast.VarScope), Funcs: make(ast.FuncScope)},
		},
	}
	p.addBuiltIns()

	return
}

func (p *Parser) addBuiltIns() {
	p.typeDefs = []ast.Type{ // TODO: better way of importing all defaults
		ast.TypeVoid,
		ast.TypeInt8,
		ast.TypeInt16,
		ast.TypeInt32,
		ast.TypeInt64,
		ast.TypeUint8,
		ast.TypeUint16,
		ast.TypeUint32,
		ast.TypeUint64,
		ast.TypeFloat32,
		ast.TypeFloat64,
		ast.TypeBool,
		ast.TypeChar,
		ast.TypeString,
	}

	anyVar := ast.Var{Name: "any", Typ: ast.SimpleType{Kind_: ast.KindAny, Size_: 0, Name_: "any"}}
	p.addFunc(ast.Func{
		Name:    "println",
		Args:    []ast.Var{anyVar},
		RetType: ast.TypeVoid,
	})

	p.addFunc(ast.Func{
		Name:    "print",
		Args:    []ast.Var{anyVar},
		RetType: ast.TypeVoid,
	})
}

func (p *Parser) get(iden ast.Identifier) ast.Expression {
	for i := len(p.currentScope) - 1; i >= 0; i-- {
		if p.currentScope[i].Contains(iden) {
			return p.currentScope[i].Get(iden)
		}
	}

	return nil
}

func (p *Parser) exists(iden ast.Identifier) bool {
	for i := len(p.currentScope) - 1; i >= 0; i-- {
		if p.currentScope[i].Contains(iden) {
			return true
		}
	}

	return false
}

func (p *Parser) defined(iden ast.Identifier) bool {
	return p.currentScope[len(p.currentScope)-1].Contains(iden)
}

func (p *Parser) addVar(v ast.Var) {
	p.currentScope[len(p.currentScope)-1].AddVar(v)
}

func (p *Parser) addFunc(f ast.Func) {
	p.currentScope[len(p.currentScope)-1].AddFunc(f)
}
func (p *Parser) ParseAll() ast.CodeBlockNode {
	result := ast.CodeBlockNode{
		Statements: make([]ast.AstNode, 0),
	}

	for statement, eof := p.ParseTopLevel(); !eof; statement, eof = p.ParseTopLevel() {
		if statement != nil {
			result.Statements = append(result.Statements, statement)
		}
	}

	result.Scope = p.currentScope[0]

	return result
}

func (p *Parser) ParseTopLevel() (ast.AstNode, bool) {
	if TTCount != 61 {
		panic("TokenType enum length changed: " + strconv.Itoa(int(TTCount)))
	}

	tok := p.lex.PeekToken()
	switch tok.typ {
	case TTConst, TTVar:
		p.lex.NextToken()

		nameTok := p.expect(TTIdentifier)
		name := ast.Identifier(nameTok.literal)

		if p.defined(name) {
			panic(nameTok.loc.String() + " ERROR: `" + nameTok.literal + "` is already defined in this scope")
		}

		p.expect(TTColon)

		typ := p.parseType()

		node := ast.VarNode{
			Var: ast.Var{
				Name:    name,
				Typ:     typ,
				IsConst: tok.typ == TTConst,
			},
			Value: nil,
		}

		if tok := p.lex.PeekToken(); tok.typ == TTAssign {
			p.lex.NextToken()
			node.Value = p.parseExpression()

			if !ast.EqualTypes(node.Typ, node.Value.ReturnType()) {
				panic(fmt.Sprintf(p.lex.curLoc.String()+" lhs and rhs types dont match: %v, %v", node.Typ, node.Value.ReturnType()))
			}

		}

		p.expect(TTSemiColon)

		p.addVar(node.Var)

		return node, false

	case TTFn:
		return p.parseFunc()

	case TTType:
		p.lex.NextToken()

		name := p.expect(TTIdentifier)

		typ := p.parseType()
		typ = typ.SetName(ast.Identifier(name.literal))

		p.typeDefs = append(p.typeDefs, typ)

		p.expect(TTSemiColon)

		return nil, false

	case TTEOF:
		return nil, true
	}

	panic(tok.loc.String() + " syntax error") // TODO: better error handling
}

func (p *Parser) parseFunctionBody() (ast.AstNode, bool) {
	if TTCount != 61 {
		panic("TokenType enum length changed: " + strconv.Itoa(int(TTCount)))
	}

	tok := p.lex.PeekToken()
	switch tok.typ {
	case TTConst, TTVar:
		p.lex.NextToken()

		nameTok := p.expect(TTIdentifier)
		name := ast.Identifier(nameTok.literal)

		if p.defined(name) {
			panic(nameTok.loc.String() + " ERROR: `" + nameTok.literal + "` is already defined in this scope")
		}

		p.expect(TTColon)

		typ := p.parseType()

		node := ast.VarNode{
			Var: ast.Var{
				Name:    name,
				Typ:     typ,
				IsConst: tok.typ == TTConst,
			},
			Value: nil,
		}

		if tok := p.lex.PeekToken(); tok.typ == TTAssign {
			p.lex.NextToken()
			node.Value = p.parseExpression()

			if !ast.EqualTypes(node.Typ, node.Value.ReturnType()) {
				panic(fmt.Sprintf(p.lex.curLoc.String()+" lhs and rhs types dont match: %v, %v", node.Typ, node.Value.ReturnType()))
			}

		}

		p.expect(TTSemiColon)

		p.addVar(node.Var)

		return node, false

	case TTFn:
		return p.parseFunc()

	case TTReturn:
		p.lex.NextToken()
		tok := p.lex.PeekToken()

		if tok.typ == TTSemiColon {
			p.lex.nextToken()

			return ast.ReturnNode{Expr: nil}, false
		}

		node := ast.ReturnNode{Expr: p.parseExpression()}
		p.expect(TTSemiColon)

		return node, false

	case TTLSquirly:
		p.lex.NextToken()
		return p.parseCodeBlock()

	case TTIdentifier, TTStar:
		node := ast.ExpressionNode{Expr: p.parseExpression()}

		p.expect(TTSemiColon)

		return node, false

	case TTSemiColon: // skip token
		p.lex.NextToken()
		return nil, false

	case TTIf:
		p.lex.NextToken()
		node := ast.IfChain{}
		node.IfCondition = p.parseExpression()

		if node.IfCondition.ReturnType().Kind() != ast.KindBool { // Kind bool instead of "real" bool because typedefed bools can also be used
			panic(p.lex.curLoc.String() + " boolean expression expected")
		}

		p.expectPeek(TTLSquirly)

		statements, eof := p.parseFunctionBody()
		if eof {
			panic("unreachable")
		}

		node.IfStatement = statements.(ast.CodeBlockNode)

		for nextToken := p.lex.PeekToken(); nextToken.typ == TTElif; nextToken = p.lex.PeekToken() {
			p.lex.NextToken()

			condition := p.parseExpression()

			if condition.ReturnType().Kind() != ast.KindBool { // same as before
				panic(p.lex.curLoc.String() + " boolean expression expected")
			}

			node.ElifConditions = append(node.ElifConditions, condition)

			p.expectPeek(TTLSquirly)

			statements, eof := p.parseFunctionBody()

			if eof {
				panic("unreachable")
			}

			node.ElifStatements = append(node.ElifStatements, statements.(ast.CodeBlockNode))
		}

		if nextToken := p.lex.PeekToken(); nextToken != nil && nextToken.typ == TTElse {
			node.HasElse = true
			p.lex.NextToken()

			p.expectPeek(TTLSquirly)

			statements, eof := p.parseFunctionBody()
			if eof {
				panic("unreachable")
			}

			node.ElseStatement = statements.(ast.CodeBlockNode)
		}

		return node, false

	case TTWhile:
		p.lex.NextToken()
		node := ast.WhileNode{}
		node.Condition = p.parseExpression()

		if node.Condition.ReturnType().Kind() != ast.KindBool { // same as in if
			panic(p.lex.curLoc.String() + " boolean expression expected")
		}

		p.expect(TTLSquirly)

		statements, eof := p.parseCodeBlock()

		if eof {
			panic("unreachable")
		}

		node.Body = statements

		return node, false

	case TTEOF:
		return nil, true
	}

	panic(tok.String() + " syntax error ") // TODO: better error handling
}

func (p *Parser) parseCodeBlock() (ast.CodeBlockNode, bool) {
	block := ast.CodeBlockNode{
		Statements: make([]ast.AstNode, 0),
		Scope:      ast.Scope{Vars: make(ast.VarScope), Funcs: make(ast.FuncScope)},
	}

	p.currentScope = append(p.currentScope, block.Scope)

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

func (p *Parser) parseFunc() (ast.FuncNode, bool) {
	p.lex.NextToken()
	node := ast.FuncNode{ReturnType: ast.TypeVoid}

	name := p.expect(TTIdentifier)
	node.Name = ast.Identifier(name.literal)
	if p.defined(node.Name) {
		panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
	}

	p.expect(TTLBrace)
	scope := ast.Scope{Vars: make(ast.VarScope), Funcs: make(ast.FuncScope)}

	for tok := p.lex.PeekToken(); tok != nil && tok.typ != TTRBrace; tok = p.lex.PeekToken() {
		arg := ast.Var{}
		arg.Name = ast.Identifier(p.expect(TTIdentifier).literal)

		p.expect(TTColon)

		arg.Typ = p.parseType()

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
		node.ReturnType = p.parseType()
	}

	p.expect(TTLSquirly)

	p.currentScope = append(p.currentScope, scope)

	body, eof := p.parseCodeBlock()
	node.Body = body

	p.currentScope = p.currentScope[:len(p.currentScope)-1]

	p.addFunc(ast.Func{Name: node.Name, Args: node.Args, RetType: node.ReturnType})

	return node, eof
}

func (p *Parser) parseType() ast.Type {
	tok := p.lex.NextToken()

	switch tok.typ {
	case TTIdentifier:
		for _, typedef := range p.typeDefs {
			if typedef.Name() == ast.Identifier(tok.literal) {
				return typedef
			}
		}

	case TTLSquare:
		p.expect(TTRSquare)
		inner := p.parseType()
		return ast.ArrayType{Name_: "", Inner: inner}

	case TTAmp:
		inner := p.parseType()
		return ast.PointerType{Name_: "", Inner: inner}

	case TTAnd: // special case for `&&` which is seen as one token
		inner := p.parseType()
		return ast.PointerType{Name_: "", Inner: ast.PointerType{Name_: "", Inner: inner}}

	case TTStruct:
		p.expect(TTLSquirly)

		typ := ast.StructType{}

		for tok := p.lex.PeekToken(); tok.typ != TTEOF && tok.typ != TTRSquirly; tok = p.lex.PeekToken() {
			field := ast.StructField{}

			field.Offset = typ.Size_
			tok := p.expect(TTIdentifier)
			field.Name = ast.Identifier(tok.literal)

			p.expect(TTColon)

			field.Typ = p.parseType()

			for _, prevField := range typ.Fields {
				if prevField.Name == field.Name {
					panic(tok.loc.String() + "ERROR: field redeclared")
				}
			}

			typ.Size_ += field.Typ.Size()

			typ.Fields = append(typ.Fields, field)

			tok = p.lex.PeekToken()

			if tok.typ != TTComma {
				break
			}

			p.lex.NextToken()
		}

		// round size up to multiple of 8
		typ.Size_ = (typ.Size_ + 7) / 8 * 8

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

func (p *Parser) parseExpression() ast.Expression {
	return p.parseBinary(0)
}

func (p *Parser) parseBinary(precedence int) ast.Expression {
	if precedence > ast.MaxPrecedence {
		return p.parseUnary()
	}

	loc := p.lex.curLoc
	lhs := p.parseBinary(precedence + 1)
	if lhs == nil {
		return nil
	}

	if ast.LeftToRight(precedence) {
		tok := p.lex.PeekToken()
		opp := tok.typ.ToBinOp()
		for tok != nil && tok.typ != TTEOF && opp != -1 && opp.Precedence() == precedence {
			p.lex.NextToken()

			rhs := p.parseBinary(precedence + 1)
			if rhs == nil {
				panic(p.lex.curLoc.String() + " opperand expected") // TODO: better error handling
			}

			if precedence == ast.BOAssign.Precedence() {
				switch lhs := lhs.(type) {
				case ast.Var:
					if !lhs.IsConst {
						goto ok1 // oh noooooo goto such unreadableness
					}
				case ast.UnaryOpNode:
					if lhs.Op == ast.UODeref {
						goto ok1
					}
				case ast.StructIndex:
					if _, ok := lhs.Base.(ast.Var); ok {
						goto ok1
					}
				case ast.ArrayIndex:
					if _, ok := lhs.Array.(ast.Var); ok {
						goto ok1
					}
				}

				panic(p.lex.curLoc.String() + " cannot assign to lhs")

			ok1:
			}

			var err error
			lhs, err = ast.NewBinaryOpNode(lhs, rhs, opp)
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

			if precedence == ast.BOAssign.Precedence() {
				switch lhs := lhs.(type) {
				case ast.Var:
					if !lhs.IsConst {
						goto ok2
					}
				case ast.UnaryOpNode:
					if lhs.Op == ast.UODeref {
						goto ok2
					}
				case ast.StructIndex:
					if _, ok := lhs.Base.(ast.Var); ok {
						goto ok2
					}
				case ast.ArrayIndex:
					if _, ok := lhs.Array.(ast.Var); ok {
						goto ok2
					}
				}

				panic(p.lex.curLoc.String() + " cannot assign to lhs")

			ok2:
			}

			var err error
			lhs, err = ast.NewBinaryOpNode(lhs, rhs, opp)
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

func (p *Parser) parseUnary() ast.Expression {
	tok := p.lex.PeekToken()
	loc := tok.loc
	opp := tok.typ.ToUnOp()

	if opp != -1 && opp.OnLeftSide() {
		p.lex.NextToken()

		expression := p.parseUnary()

		if expression == nil {
			panic(p.lex.curLoc.String() + " Error: opperand expected")
		}

		unOp, err := ast.NewUnaryOpNode(expression, opp)

		if err != nil {
			panic(loc.String() + err.Error())
		}

		return unOp
	}

	expression := p.parsePrimary()

	if expression == nil {
		panic(p.lex.curLoc.String() + " Error: opperand expected")
	}

loop:
	for tok := p.lex.PeekToken(); ; tok = p.lex.PeekToken() {
		switch tok.typ {
		case TTPeriod:
			p.lex.NextToken()

			if expression.ReturnType().Kind() != ast.KindStruct {
				panic(p.lex.curLoc.String() + "ERROR: not a struct")
			}

			tok = p.lex.PeekToken()

			if tok.typ == TTLSquirly {
				panic("struct initialization")
			}

			p.expect(TTIdentifier)

			optField := expression.ReturnType().(ast.StructType).GetField(ast.Identifier(tok.literal))

			if optField == nil {
				fmt.Println(expression.ReturnType())
				panic(p.lex.curLoc.String() + "ERROR: no field `" + tok.literal + "`")
			}

			field := *optField

			if curVal, ok := expression.(ast.StructIndex); ok {
				curVal.Offset += field.Offset
				curVal.Typ = field.Typ
				expression = curVal
			} else {
				expression = ast.StructIndex{
					Base:   expression,
					Typ:    field.Typ,
					Offset: field.Offset,
				}
			}

		case TTLBrace:
			p.lex.NextToken()
			funcCall := ast.FuncCall{}

			if val, ok := expression.(ast.Func); ok {
				funcCall.Fun = val
			} else {
				fmt.Println(val)
				panic(p.lex.curLoc.String() + " Can only call functions")
			}

			argIdx := -1

			for tok := p.lex.PeekToken(); tok.typ != TTRBrace; tok = p.lex.PeekToken() {
				if len(funcCall.Args) >= len(funcCall.Fun.Args) {
					panic(p.lex.curLoc.String() + " Error: to many arguments to function")
				}

				loc := p.lex.curLoc
				arg := p.parseExpression()

				funcCall.Args = append(funcCall.Args, arg)
				argIdx++

				expectedType := funcCall.Fun.Args[argIdx].ReturnType()

				if !ast.EqualTypes(arg.ReturnType(), expectedType) && expectedType.Kind() != ast.KindAny {
					panic(loc.String() + " Error: argument type mismatch")
				}

				tok = p.lex.PeekToken()
				if tok == nil || tok.typ != TTComma {
					break
				}

				p.lex.NextToken()
			}

			if len(funcCall.Args) < len(funcCall.Fun.Args) {
				panic(p.lex.curLoc.String() + " Error: to few arguments to function")
			}

			p.expect(TTRBrace)

			expression = funcCall

		case TTLSquare:
			p.lex.NextToken()
			index := p.parseExpression()
			p.expect(TTRSquare)
			expression = ast.ArrayIndex{
				Array: expression,
				Index: index,
				Typ:   expression.ReturnType().(ast.ArrayType).Inner,
			}

		default:
			opp = tok.typ.ToUnOp()

			if opp == -1 || opp.OnLeftSide() {
				break loop
			}

			p.lex.NextToken()
			var err error
			expression, err = ast.NewUnaryOpNode(expression, opp)

			if err != nil {
				panic(p.lex.curLoc.String() + err.Error())
			}
		}
	}

	return expression
}

func (p *Parser) parsePrimary() ast.Expression {
	tok := p.lex.PeekToken()
	if tok.typ == TTEOF {
		panic("opperand expected")
	}

	switch tok.typ {
	case TTIdentifier:
		p.lex.NextToken()
		if tok.literal == "alloc" {
			return p.parseAlloc()
		}

		if !p.exists(ast.Identifier(tok.literal)) {
			panic(tok.loc.String() + " Undefinded name: " + tok.literal)
		}

		return p.get(ast.Identifier(tok.literal))

	case TTInt:
		p.lex.NextToken()
		val := tok.extraInfo.(int)
		return ast.IntLit{Value: int64(val)}

	case TTFloat:
		p.lex.NextToken()
		val := tok.extraInfo.(float64)
		return ast.FloatLit{Value: val}

	case TTString:
		p.lex.NextToken()
		return ast.StrLit{Value: tok.extraInfo.(string), Litteral: tok.literal}

	case TTChar:
		p.lex.NextToken()
		return ast.CharLit{Value: tok.extraInfo.(rune), Litteral: tok.literal}

	case TTTrue:
		p.lex.NextToken()
		return ast.BoolLit{Value: true}

	case TTFalse:
		p.lex.NextToken()
		return ast.BoolLit{Value: false}

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
			resultKind := result.ReturnType().Kind()
			sameKind := innerKind == resultKind
			intToUint := (innerKind == ast.KindUint && resultKind == ast.KindInt) ||
				(typ.Kind() == ast.KindInt && resultKind == ast.KindUint)

			if !sameKind && !intToUint && innerKind != ast.KindStruct {
				panic(fmt.Errorf("%s ERROR: cannot cast: incompatible types", p.lex.curLoc.String()))
			}

			p.expect(TTRBrace)

			return ast.Cast{Inner: result, NewType: typ}
		}

		p.expect(TTRBrace)
	}

	panic(p.lex.NextToken().String() + " illegal token") // TODO: better error handling
}

func (p *Parser) parseAlloc() ast.Expression {
	p.expect(TTLBrace)

	typ := p.parseType()

	var result ast.Allocation

	if typ.Kind() == ast.KindArray {
		p.expect(TTComma)

		count := p.parseExpression()

		if (count.ReturnType().Kind() != ast.KindInt &&
			count.ReturnType().Kind() != ast.KindUint) ||
			count.ReturnType().Size() != 8 {
			panic("type mismatch")
		}

		result = ast.Allocation{Typ: typ, ElemsCount: count}
	} else {
		result = ast.Allocation{
			Typ:        ast.PointerType{Name_: "", Inner: typ},
			ElemsCount: ast.IntLit{Value: 1},
		}
	}

	p.expect(TTRBrace)

	return result
}
