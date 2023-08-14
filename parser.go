package main

import (
	"strconv"
)

type Parser struct {
	lex          *Lexer
	currentScope []*Scope
}

func ParserFromFilename(filename string) *Parser {
	return &Parser{
		lex: LexerFromFilename(filename),
		currentScope: []*Scope{
			{make(VarScope), make(ConstScope), make(FuncScope)},
		},
	}
}

func ParserFromString(str string) *Parser {
	return &Parser{
		lex: LexerFromString(str),
		currentScope: []*Scope{
			{make(VarScope), make(ConstScope), make(FuncScope)},
		},
	}
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

func (p *Parser) addConst(c Const) {
	p.currentScope[len(p.currentScope)-1].AddConst(c)
}

func (p *Parser) addFunc(f Func) {
	p.currentScope[len(p.currentScope)-1].AddFunc(f)
}

func (p *Parser) ParseTopLevel() (AstNode, bool) {
	if TTCount != 59 {
		panic("TokenType enum length changed: " + strconv.Itoa(int(TTCount)))
	}

	tok := p.lex.PeekToken()
	switch tok.typ {
	case TTConst:
		p.lex.NextToken()
		name := p.expect(TTIdentifier)
		p.expect(TTColon)
		typ := p.expect(TTIdentifier)

		if tok := p.lex.PeekToken(); tok.typ != TTAssign {
			p.expect(TTSemiColon)

			if p.defined(Identifier(name.literal)) {
				panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
			}

			node := ConstNode{
				name: Identifier(name.literal),
				typ:  Type{KindFromString(typ.literal), Identifier(typ.literal)},
			}

			p.addConst(Const{node.name, node.typ})

			return node, false
		}

		p.expect(TTAssign)

		node := ConstNode{
			name:  Identifier(name.literal),
			typ:   Type{KindFromString(typ.literal), Identifier(typ.literal)},
			Value: p.parseExpression(),
		}

		p.expect(TTSemiColon)

		if p.defined(Identifier(name.literal)) {
			panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
		}

		p.addConst(Const{node.name, node.typ})

		return node, false

	case TTVar:
		p.lex.NextToken()
		name := p.expect(TTIdentifier)
		p.expect(TTColon)
		typ := p.expect(TTIdentifier)

		if tok := p.lex.PeekToken(); tok.typ != TTAssign {
			p.expect(TTSemiColon)
			if p.defined(Identifier(name.literal)) {
				panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
			}
			node := VarNode{
				name: Identifier(name.literal),
				typ:  Type{KindFromString(typ.literal), Identifier(typ.literal)},
			}

			p.addVar(Var{node.name, node.typ})

			return node, false
		}

		p.expect(TTAssign)

		node := VarNode{
			name:  Identifier(name.literal),
			typ:   Type{KindFromString(typ.literal), Identifier(typ.literal)},
			Value: p.parseExpression(),
		}
		p.expect(TTSemiColon)

		if p.defined(Identifier(name.literal)) {
			panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
		}

		p.addVar(Var{node.name, node.typ})

		return node, false

	case TTFn:
		return p.parseFunc()
	case TTType:
		panic("not implemented")

	case TTEOF:
		return nil, true
	}

	panic(tok.loc.String() + " syntax error") // TODO: better error handling
}

func (p *Parser) parseFunctionBody() (AstNode, bool) {
	if TTCount != 59 {
		panic("TokenType enum length changed: " + strconv.Itoa(int(TTCount)))
	}

	tok := p.lex.PeekToken()
	switch tok.typ {
	case TTConst:
		p.lex.NextToken()
		name := p.expect(TTIdentifier)
		p.expect(TTColon)
		typ := p.expect(TTIdentifier)

		if tok := p.lex.PeekToken(); tok.typ != TTAssign {
			p.expect(TTSemiColon)
			if p.defined(Identifier(name.literal)) {
				panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
			}
			node := ConstNode{
				name: Identifier(name.literal),
				typ:  Type{KindFromString(typ.literal), Identifier(typ.literal)},
			}

			p.addConst(Const{node.name, node.typ})

			return node, false
		}

		p.expect(TTAssign)

		node := ConstNode{
			name:  Identifier(name.literal),
			typ:   Type{KindFromString(typ.literal), Identifier(typ.literal)},
			Value: p.parseExpression(),
		}

		p.expect(TTSemiColon)

		if p.defined(Identifier(name.literal)) {
			panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
		}

		p.addConst(Const{node.name, node.typ})
		return node, false

	case TTVar:
		p.lex.NextToken()
		name := p.expect(TTIdentifier)
		p.expect(TTColon)
		typ := p.expect(TTIdentifier)

		if tok := p.lex.PeekToken(); tok.typ != TTAssign {
			p.expect(TTSemiColon)
			if p.defined(Identifier(name.literal)) {
				panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
			}
			return VarNode{
				name: Identifier(name.literal),
				typ:  Type{KindFromString(typ.literal), Identifier(typ.literal)},
			}, false
		}

		p.expect(TTAssign)

		node := VarNode{
			name:  Identifier(name.literal),
			typ:   Type{KindFromString(typ.literal), Identifier(typ.literal)},
			Value: p.parseExpression(),
		}

		p.expect(TTSemiColon)

		if p.defined(Identifier(name.literal)) {
			panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
		}

		p.addVar(Var{node.name, node.typ})
		return node, false

	case TTFn:
		return p.parseFunc()

	case TTReturn:
		p.lex.NextToken()
		node := ReturnNode{p.parseExpression()}
		p.expect(TTSemiColon)

		return node, false

	case TTLSquirly:
		p.lex.NextToken()
		scope := Scope{make(VarScope), make(ConstScope), make(FuncScope)}
		return p.parseCodeBlock(scope)

	case TTIdentifier:
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
		p.expectPeek(TTLSquirly)

		statements, eof := p.parseFunctionBody()
		if eof {
			panic("unreachable")
		}

		node.IfStatement = statements.(CodeBlockNode)

		for nextToken := p.lex.PeekToken(); nextToken.typ == TTElif; nextToken = p.lex.PeekToken() {
			p.lex.NextToken()

			condition := p.parseExpression()
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

	case TTEOF:
		return nil, true
	}

	panic(tok.String() + " syntax error ") // TODO: better error handling
}

func (p *Parser) parseCodeBlock(scope Scope) (CodeBlockNode, bool) {
	var block CodeBlockNode

	// if scope is empty
	if scope.vars == nil {
		block = CodeBlockNode{make([]AstNode, 0), Scope{make(VarScope), make(ConstScope), make(FuncScope)}}
	} else {
		block = CodeBlockNode{make([]AstNode, 0), scope}
	}

	p.currentScope = append(p.currentScope, &block.scope)

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
	node := FuncNode{}

	name := p.expect(TTIdentifier)
	node.name = Identifier(name.literal)
	if p.defined(node.name) {
		panic(name.loc.String() + " ERROR: `" + name.literal + "` is already defined in this scope")
	}

	p.expect(TTLBrace)
	scope := Scope{make(VarScope), make(ConstScope), make(FuncScope)}

	for tok := p.lex.PeekToken(); tok != nil && tok.typ != TTRBrace; tok = p.lex.PeekToken() {
		arg := Var{}
		arg.name = Identifier(p.expect(TTIdentifier).literal)

		p.expect(TTColon)

		arg.typ.name = Identifier(p.expect(TTIdentifier).literal)
		arg.typ.kind = KindFromString(string(arg.typ.name))

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
		node.returnType.name = Identifier(p.lex.NextToken().literal)
		node.returnType.kind = KindFromString(string(node.returnType.name))
	}

	p.expect(TTLSquirly)

	body, eof := p.parseCodeBlock(scope)
	node.Body = body

	p.addFunc(Func{node.name, node.Args, node.returnType})

	return node, eof
}

func (p *Parser) expect(typ TokenType) *Token {
	tok := p.lex.NextToken()
	if tok.typ != typ {
		panic(tok.loc.String() + " " + tok.literal + " syntax error expected: " + typ.String()) // TODO: better error handling
	}

	return tok
}

func (p *Parser) expectPeek(typ TokenType) *Token {
	tok := p.lex.PeekToken()
	if tok.typ != typ {
		panic(tok.loc.String() + " " + tok.literal + " syntax error expected: " + typ.String()) // TODO: better error handling
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

	lhs := p.parseBinary(precedence + 1)
	if lhs == nil {
		return nil
	}

	if LeftToRight(precedence) {
		tok := p.lex.PeekToken()
		opp := tok.typ.TokenTypeToBinOp()
		for tok != nil && tok.typ != TTEOF && opp != -1 && opp.Precedence() == precedence {
			p.lex.NextToken()

			rhs := p.parseBinary(precedence + 1)
			if rhs == nil {
				panic(p.lex.curLoc.String() + " opperand expected") // TODO: better error handling
			}

			if precedence == BOAssign.Precedence() {
				switch lhs.(type) {
				case VarLit:
				default:
					panic(p.lex.curLoc.String() + " Error: can only assign to variable")
				}
			}

			lhs = BinaryOpNode{lhs, rhs, opp}
			tok = p.lex.PeekToken()
			opp = tok.typ.TokenTypeToBinOp()
		}
	} else {
		tok := p.lex.PeekToken()
		opp := tok.typ.TokenTypeToBinOp()
		for tok.typ != TTEOF && opp != -1 && opp.Precedence() == precedence {
			p.lex.NextToken()
			rhs := p.parseBinary(precedence)
			if rhs == nil {
				panic(p.lex.curLoc.String() + " opperand expected") // TODO: better error handling
			}

			if precedence == BOAssign.Precedence() {
				switch lhs.(type) {
				case VarLit:
				default:
					panic(p.lex.curLoc.String() + " Error: can only assign to variable")
				}
			}

			lhs = BinaryOpNode{lhs, rhs, opp}
			tok = p.lex.PeekToken()
			opp = tok.typ.TokenTypeToBinOp()
		}
	}

	return lhs
}

// TODO: unary expressions
func (p *Parser) parseUnary() Expression {
	return p.parsePrimary()
}

func (p *Parser) parsePrimary() Expression {
	tok := p.lex.PeekToken()
	if tok.typ == TTEOF {
		panic("opperand expected")
	}

	switch tok.typ {
	case TTIdentifier:
		p.lex.NextToken()

		if next := p.lex.PeekToken(); next == nil || next.typ != TTLBrace {
			return VarLit{Identifier(tok.literal)}
		}

		expr := FuncCall{funcName: Identifier(tok.literal)}

		p.lex.NextToken() // always`(` because of if condition

		for tok := p.lex.PeekToken(); tok != nil && tok.typ != TTRBrace; tok = p.lex.PeekToken() {
			expr.Args = append(expr.Args, p.parseExpression())

			tok = p.lex.PeekToken()
			if tok == nil || tok.typ != TTComma {
				break
			}

			p.lex.NextToken()
		}

		p.expect(TTRBrace)

		return expr

	case TTInt:
		p.lex.NextToken()
		val, _ := strconv.Atoi(tok.literal)
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

		if p.lex.NextToken().typ != TTRBrace {
			panic(p.lex.curLoc.String() + " `)` expected") // TODO: better error handling
		}

		return result
	}

	panic(p.lex.NextToken().String() + " illegal token") // TODO: better error handling
}
