package main

import (
	"strconv"
)

type Parser struct {
	lex *Lexer
}

func (p *Parser) ParseTopLevel() (node AstNode) {
	a := 3
	print(a)
	if TTCount != 58 {
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

		node = ConstNode{
			name:  Identifier(nameToken.literal),
			typ:   Type(typeToken.literal),
			value: p.parseExpression(),
		}
		p.expect(TTSemiColon)

		return

	case TTVar:
		p.lex.NextToken()
		nameToken := p.expect(TTIdentifier)
		_ = p.expect(TTColon)
		typeToken := p.expect(TTIdentifier)
		_ = p.expect(TTAssign)

		node = VarNode{
			name:  Identifier(nameToken.literal),
			typ:   Type(typeToken.literal),
			value: p.parseExpression(),
		}
		p.expect(TTSemiColon)
		return

	case TTFn:
		panic("not implemented")
	case TTType:
		panic("not implemented")

	case TTComment: // skip token
	case TTEOF:
		return nil
	}

	panic(tok.loc.String() + " syntax error") // TODO: better error handling
}

func (p *Parser) parseFunctionBody() (node AstNode) {
	if TTCount != 58 {
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

		node = ConstNode{
			name:  Identifier(nameToken.literal),
			typ:   Type(typeToken.literal),
			value: p.parseExpression(),
		}

		p.expect(TTSemiColon)

		return

	case TTVar:
		p.lex.NextToken()
		nameToken := p.expect(TTIdentifier)
		p.expect(TTColon)
		typeToken := p.expect(TTIdentifier)
		p.expect(TTAssign)

		node = VarNode{
			name:  Identifier(nameToken.literal),
			typ:   Type(typeToken.literal),
			value: p.parseExpression(),
		}

		p.expect(TTSemiColon)
		return

	case TTFn:
		panic("function objects will come later")
	case TTLSquirly:
		panic("not implemented")

	case TTIdentifier:
		node = p.parseExpression()

		p.expect(TTSemiColon)
		return

	case TTSemiColon, TTComment: // skip token
		p.lex.NextToken()
		return p.parseFunctionBody()
	case TTEOF:
		return nil
	}

	panic(tok.String() + " syntax error ") // TODO: better error handling
}

func (p *Parser) expect(typ TokenType) *Token {
	tok := p.lex.NextToken()
	if tok.typ != typ {
		panic(tok.loc.String() + " syntax error") // TODO: better error handling
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
		return VarLit{tok.literal}
	case TTNumber:
		p.lex.NextToken()
		val, _ := strconv.Atoi(tok.literal)
		return IntLit{val}
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
