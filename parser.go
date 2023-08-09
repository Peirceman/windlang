package main

import (
	"strconv"
)

type Parser struct {
	lex *Lexer
}

func (p *Parser) ParseTopLevel() (AstNode, bool) {
	if TTCount != 59 {
		panic("TokenType enum length changed: " + strconv.Itoa(int(TTCount)))
	}

	tok := p.lex.PeekToken()
	switch tok.typ {
	case TTConst:
		p.lex.NextToken()
		nameToken := p.expect(TTIdentifier)
		_ = p.expect(TTColon)
		typeToken := p.expect(TTIdentifier)
		_ = p.expect(TTAssign)

		node := ConstNode{
			name:  Identifier(nameToken.literal),
			typ:   Type(typeToken.literal),
			value: p.parseExpression(),
		}

		p.expect(TTSemiColon)

		return node, false

	case TTVar:
		p.lex.NextToken()
		nameToken := p.expect(TTIdentifier)
		_ = p.expect(TTColon)
		typeToken := p.expect(TTIdentifier)
		_ = p.expect(TTAssign)

		node := VarNode{
			name:  Identifier(nameToken.literal),
			typ:   Type(typeToken.literal),
			value: p.parseExpression(),
		}
		p.expect(TTSemiColon)

		return node, false

	case TTFn:
		return p.parseFunc()
	case TTType:
		panic("not implemented")

	case TTComment: // skip token
		p.lex.NextToken()
		return nil, false

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
		nameToken := p.expect(TTIdentifier)
		p.expect(TTColon)
		typeToken := p.expect(TTIdentifier)
		p.expect(TTAssign)

		node := ConstNode{
			name:  Identifier(nameToken.literal),
			typ:   Type(typeToken.literal),
			value: p.parseExpression(),
		}

		p.expect(TTSemiColon)

		return node, false

	case TTVar:
		p.lex.NextToken()
		nameToken := p.expect(TTIdentifier)
		p.expect(TTColon)
		typeToken := p.expect(TTIdentifier)
		p.expect(TTAssign)

		node := VarNode{
			name:  Identifier(nameToken.literal),
			typ:   Type(typeToken.literal),
			value: p.parseExpression(),
		}

		p.expect(TTSemiColon)

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
		block := CodeBlockNode{make([]AstNode, 0)}

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
		return block, false

	case TTIdentifier:
		node := ExpressionNode{p.parseExpression()}

		p.expect(TTSemiColon)

		return node, false

	case TTSemiColon, TTComment: // skip token
		p.lex.NextToken()
		return nil, false

	case TTIf:
		p.lex.NextToken()
		node := IfChain{}
		node.ifCondition = p.parseExpression()
		p.expectPeek(TTLSquirly)

		statements, eof := p.parseFunctionBody()
		if eof {
			panic("unreachable")
		}

		node.ifStatement = statements.(CodeBlockNode)

		for nextToken := p.lex.PeekToken(); nextToken.typ == TTElif; nextToken = p.lex.PeekToken() {
			p.lex.NextToken()

			condition := p.parseExpression()
			node.elifConditions = append(node.elifConditions, condition)

			p.expectPeek(TTLSquirly)

			statements, eof := p.parseFunctionBody()
			if eof {
				panic("unreachable")
			}

			node.elifStatements = append(node.elifStatements, statements.(CodeBlockNode))
		}

		if nextToken := p.lex.PeekToken(); nextToken != nil && nextToken.typ == TTElse {
			node.hasElse = true
			p.lex.NextToken()

			p.expectPeek(TTLSquirly)

			statements, eof := p.parseFunctionBody()
			if eof {
				panic("unreachable")
			}

			node.elseStatement = statements.(CodeBlockNode)
		}

		return node, false

	case TTEOF:
		return nil, true
	}

	panic(tok.String() + " syntax error ") // TODO: better error handling
}

func (p *Parser) parseFunc() (FnNode, bool) {
	p.lex.NextToken()
	node := FnNode{}

	node.name = Identifier(p.expect(TTIdentifier).literal)
	p.expect(TTLBrace)

	for tok := p.lex.PeekToken(); tok != nil && tok.typ != TTRBrace; tok = p.lex.PeekToken() {
		arg := Var{}
		arg.name = Identifier(p.expect(TTIdentifier).literal)

		p.expect(TTColon)

		arg.typ = Type(p.expect(TTIdentifier).literal)

		node.Args = append(node.Args, arg)

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
		node.returnType = Type(p.lex.NextToken().literal)
	}

	p.expectPeek(TTLSquirly)

	body, eof := p.parseFunctionBody()
	node.body = body.(CodeBlockNode)

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
		for (tok != nil && tok.typ != TTEOF && opp != -1 && opp.Precedence() == precedence) || tok.typ == TTComment {
			p.lex.NextToken()
			if tok.typ == TTComment {
				continue
			}

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

	case TTComment:
		p.lex.NextToken()
		return p.parsePrimary()
	}

	panic(p.lex.NextToken().String() + " illegal token") // TODO: better error handling
}
