package main

import (
	"fmt"
	"strconv"
	"windlang/ast"
	"windlang/lexer"
)

type Parser struct {
	lex          *lexer.Lexer
	currentScope []ast.Scope
	typeDefs     []ast.Type
}

func ParserFromFilename(filename string) (p *Parser) {
	p = &Parser{
		lex: lexer.LexerFromFilename(filename),
		currentScope: []ast.Scope{
			{Vars: make(ast.VarScope), Funcs: make(ast.FuncScope)},
		},
	}

	p.addBuiltIns()

	return
}

func ParserFromString(str string) (p *Parser) {
	p = &Parser{
		lex: lexer.LexerFromString(str),
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
	if lexer.TTCount != 60 {
		panic("TokenType enum length changed: " + strconv.Itoa(int(lexer.TTCount)))
	}

	tok := p.lex.PeekToken()
	startLoc := tok.Loc
	switch tok.Typ {
	case lexer.TTConst, lexer.TTVar:
		p.lex.NextToken()

		nameTok := p.expect(lexer.TTIdentifier)
		name := ast.Identifier(nameTok.Literal)

		if p.defined(name) {
			panic(nameTok.Loc.String() + " ERROR: `" + nameTok.Literal + "` is already defined in this scope")
		}

		p.expect(lexer.TTColon)

		typ := p.parseType()

		node := ast.VarNode{
			Var: ast.Var{
				Name:    name,
				Typ:     typ,
				IsConst: tok.Typ == lexer.TTConst,
			},
			Value: nil,
			Loc_:  startLoc,
		}

		if tok := p.lex.PeekToken(); tok.Typ == lexer.TTAssign {
			p.lex.NextToken()
			node.Value = p.parseExpression()
		}

		p.expect(lexer.TTSemiColon)

		p.addVar(node.Var)

		return node, false

	case lexer.TTFn:
		return p.parseFunc()

	case lexer.TTType:
		p.lex.NextToken()

		name := p.expect(lexer.TTIdentifier)

		typ := p.parseType()
		typ = typ.SetName(ast.Identifier(name.Literal))

		p.typeDefs = append(p.typeDefs, typ)

		p.expect(lexer.TTSemiColon)

		return nil, false

	case lexer.TTEOF:
		return nil, true

	default:
		panic(tok.Loc.String() + "ERROR: invalid syntax")
	}
}

func (p *Parser) parseFunctionBody() (result ast.AstNode, eof bool) {
	if lexer.TTCount != 60 {
		panic("TokenType enum length changed: " + strconv.Itoa(int(lexer.TTCount)))
	}

	tok := p.lex.PeekToken()
	startLoc := tok.Loc
	switch tok.Typ {
	case lexer.TTConst, lexer.TTVar:
		p.lex.NextToken()

		nameTok := p.expect(lexer.TTIdentifier)
		name := ast.Identifier(nameTok.Literal)

		if p.defined(name) {
			panic(nameTok.Loc.String() + " ERROR: `" + nameTok.Literal + "` is already defined in this scope")
		}

		p.expect(lexer.TTColon)

		typ := p.parseType()

		node := ast.VarNode{
			Var: ast.Var{
				Name:    name,
				Typ:     typ,
				IsConst: tok.Typ == lexer.TTConst,
			},
			Value: nil,
			Loc_:  startLoc,
		}

		if tok := p.lex.PeekToken(); tok.Typ == lexer.TTAssign {
			p.lex.NextToken()
			node.Value = p.parseExpression()
		}

		p.expect(lexer.TTSemiColon)

		p.addVar(node.Var)

		return node, false

	case lexer.TTFn:
		return p.parseFunc()

	case lexer.TTReturn:
		p.lex.NextToken()
		tok := p.lex.PeekToken()

		if tok.Typ == lexer.TTSemiColon {
			p.lex.NextToken()

			return ast.ReturnNode{Expr: nil, Loc_: startLoc}, false
		}

		node := ast.ReturnNode{Expr: p.parseExpression(), Loc_: startLoc}

		p.expect(lexer.TTSemiColon)

		return node, false

	case lexer.TTLSquirly:
		return p.parseCodeBlock()

	case lexer.TTIdentifier, lexer.TTStar:
		node := ast.ExpressionNode{Expr: p.parseExpression()}

		p.expect(lexer.TTSemiColon)

		return node, false

	case lexer.TTSemiColon: // skip token
		p.lex.NextToken()

		return nil, false

	case lexer.TTIf:
		p.lex.NextToken()
		node := ast.IfChain{Loc_: startLoc}
		node.Conditions = append(node.Conditions, p.parseExpression())

		statements, eof := p.parseCodeBlock()
		if eof {
			panic("unreachable")
		}

		node.Statements = append(node.Statements, statements)

		wasElseIf := true
		for nextToken := p.lex.PeekToken(); nextToken.Typ == lexer.TTElse && wasElseIf; nextToken = p.lex.PeekToken() {
			p.lex.NextToken()

			if p.lex.PeekToken().Typ == lexer.TTIf {
				p.lex.NextToken()

				condition := p.parseExpression()

				node.Conditions = append(node.Conditions, condition)
			} else {
				wasElseIf = false
			}

			statements, eof := p.parseCodeBlock()

			if eof {
				panic("unreachable")
			}

			node.Statements = append(node.Statements, statements)
		}

		return node, false

	case lexer.TTWhile:
		p.lex.NextToken()
		node := ast.WhileNode{Loc_: startLoc}
		node.Condition = p.parseExpression()

		statements, eof := p.parseCodeBlock()

		if eof {
			panic("unreachable")
		}

		node.Body = statements

		return node, false

	case lexer.TTEOF:
		return nil, true

	default:
		panic(tok.String() + " syntax error ") // TODO: better error handling
	}
}

func (p *Parser) parseCodeBlock() (ast.CodeBlockNode, bool) {
	p.expect(lexer.TTLSquirly)

	block := ast.CodeBlockNode{
		Statements: make([]ast.AstNode, 0),
		Scope:      ast.Scope{Vars: make(ast.VarScope), Funcs: make(ast.FuncScope)},
	}

	p.currentScope = append(p.currentScope, block.Scope)

	for tok := p.lex.PeekToken(); tok.Typ != lexer.TTEOF && tok.Typ != lexer.TTRSquirly; tok = p.lex.PeekToken() {
		statement, eof := p.parseFunctionBody()
		if eof {
			panic(tok.Loc.String() + " `}` excpected")
		}

		if statement == nil {
			continue
		}

		block.Statements = append(block.Statements, statement)
	}

	block.Statements = block.Statements[:len(block.Statements):len(block.Statements)]
	p.expect(lexer.TTRSquirly)

	p.currentScope = p.currentScope[:len(p.currentScope)-1]

	return block, false
}

func (p *Parser) parseFunc() (ast.AstNode, bool) {
	p.lex.NextToken()
	node := ast.FuncNode{ReturnType: ast.TypeVoid}

	name := p.expect(lexer.TTIdentifier)
	node.Name = ast.Identifier(name.Literal)
	if p.defined(node.Name) {
		panic(name.Loc.String() + " ERROR: `" + name.Literal + "` is already defined in this scope")
	}

	p.expect(lexer.TTLBrace)
	scope := ast.Scope{Vars: make(ast.VarScope), Funcs: make(ast.FuncScope)}

	for tok := p.lex.PeekToken(); tok != nil && tok.Typ != lexer.TTRBrace; tok = p.lex.PeekToken() {
		arg := ast.Var{}
		arg.Name = ast.Identifier(p.expect(lexer.TTIdentifier).Literal)

		p.expect(lexer.TTColon)

		arg.Typ = p.parseType()

		node.Args = append(node.Args, arg)
		scope.AddVar(arg)

		tok = p.lex.PeekToken()
		if tok == nil || tok.Typ != lexer.TTComma {
			break
		}

		p.lex.NextToken()
	}

	p.expect(lexer.TTRBrace)

	tok := p.lex.PeekToken()
	if tok.Typ == lexer.TTColon {
		p.lex.NextToken()
		node.ReturnType = p.parseType()
	}

	p.currentScope = append(p.currentScope, scope)

	body, eof := p.parseCodeBlock()
	node.Body = body

	p.currentScope = p.currentScope[:len(p.currentScope)-1]

	p.addFunc(ast.Func{Name: node.Name, Args: node.Args, RetType: node.ReturnType})

	return node, eof
}

func (p *Parser) parseType() ast.Type {
	tok := p.lex.NextToken()

	switch tok.Typ {
	case lexer.TTIdentifier:
		for _, typedef := range p.typeDefs {
			if typedef.Name() == ast.Identifier(tok.Literal) {
				return typedef
			}
		}

	case lexer.TTLSquare:
		p.expect(lexer.TTRSquare)
		inner := p.parseType()
		return ast.ArrayType{Name_: "", Inner: inner}

	case lexer.TTAmp:
		inner := p.parseType()
		return ast.PointerType{Name_: "", Inner: inner}

	case lexer.TTAnd: // special case for `&&` which is seen as one token
		inner := p.parseType()
		return ast.PointerType{Name_: "", Inner: ast.PointerType{Name_: "", Inner: inner}}

	case lexer.TTStruct:
		p.expect(lexer.TTLSquirly)

		typ := ast.StructType{}

		for tok := p.lex.PeekToken(); tok.Typ != lexer.TTEOF && tok.Typ != lexer.TTRSquirly; tok = p.lex.PeekToken() {
			field := ast.StructField{}

			field.Offset = typ.Size_
			tok := p.expect(lexer.TTIdentifier)
			field.Name = ast.Identifier(tok.Literal)

			p.expect(lexer.TTColon)

			field.Typ = p.parseType()

			for _, prevField := range typ.Fields {
				if prevField.Name == field.Name {
					panic(tok.Loc.String() + "ERROR: field redeclared")
				}
			}

			typ.Size_ += field.Typ.Size()

			typ.Fields = append(typ.Fields, field)

			tok = p.lex.PeekToken()

			if tok.Typ != lexer.TTComma {
				break
			}

			p.lex.NextToken()
		}

		// round size up to multiple of 8
		typ.Size_ = (typ.Size_ + 7) / 8 * 8

		p.expect(lexer.TTRSquirly)

		return typ
	}

	panic(tok.Loc.String() + " Error: expected type")
}

func (p *Parser) expect(typ lexer.TokenType) *lexer.Token {
	tok := p.lex.NextToken()
	if tok.Typ != typ {
		panic(tok.Loc.String() + " " + tok.Literal + " syntax error, expected: " + typ.String()) // TODO: better error handling
	}

	return tok
}

func (p *Parser) expectPeek(typ lexer.TokenType) *lexer.Token {
	tok := p.lex.PeekToken()
	if tok.Typ != typ {
		panic(tok.Loc.String() + " " + tok.Literal + " syntax error, expected: " + typ.String()) // TODO: better error handling
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

	lhs := p.parseBinary(precedence + 1)
	tok := p.lex.PeekToken()
	opp := ToBinOp(tok.Typ)

	if lhs == nil {
		return nil
	}

	if ast.LeftToRight(precedence) {
		for tok != nil && tok.Typ != lexer.TTEOF && opp != -1 && opp.Precedence() == precedence {
			p.lex.NextToken()

			rhs := p.parseBinary(precedence + 1)

			lhs = ast.BinaryOpNode{Lhs: lhs, Rhs: rhs, Op: opp, Loc_: lhs.Loc()}
			tok = p.lex.PeekToken()
			opp = ToBinOp(tok.Typ)
		}
	} else {
		for tok.Typ != lexer.TTEOF && opp != -1 && opp.Precedence() == precedence {
			p.lex.NextToken()
			rhs := p.parseBinary(precedence)

			lhs = ast.BinaryOpNode{Lhs: lhs, Rhs: rhs, Op: opp, Loc_: lhs.Loc()}
			tok = p.lex.PeekToken()
			opp = ToBinOp(tok.Typ)
		}
	}

	return lhs
}

func (p *Parser) parseUnary() ast.Expression {
	tok := p.lex.PeekToken()
	loc := tok.Loc
	opp := ToUnOp(tok.Typ)

	if opp != -1 && opp.OnLeftSide() {
		p.lex.NextToken()

		expression := p.parseUnary()

		if expression == nil {
			panic(tok.Loc.String() + "Error: opperand expected")
		}

		unOp, err := newUnaryOpNode(expression, opp, loc)

		if err != nil {
			panic(loc.String() + err.Error())
		}

		return unOp
	}

	expression := p.parsePrimary()

	if expression == nil {
		panic(tok.Loc.String() + "Error: opperand expected")
	}

loop:
	for tok := p.lex.PeekToken(); ; tok = p.lex.PeekToken() {
		switch tok.Typ {
		case lexer.TTPeriod:
			p.lex.NextToken()

			tok = p.lex.PeekToken()

			if tok.Typ == lexer.TTLSquirly {
				panic("struct initialization")
			}

			p.expect(lexer.TTIdentifier)

			expression = &ast.StructIndex{
				Base: expression,
				FieldName: ast.Identifier(tok.Literal),
				FieldLoc: tok.Loc,
			}

		case lexer.TTLBrace:
			p.lex.NextToken()
			funcCall := ast.FuncCall{}

			if val, ok := expression.(ast.Func); ok {
				funcCall.Fun = val
				funcCall.Loc_ = val.Loc_
			} else {
				fmt.Println(val)
				panic(tok.Loc.String() + " Can only call functions")
			}

			argIdx := -1

			for tok := p.lex.PeekToken(); tok.Typ != lexer.TTRBrace; tok = p.lex.PeekToken() {
				arg := p.parseExpression()

				funcCall.Args = append(funcCall.Args, arg)
				argIdx++

				tok = p.lex.PeekToken()
				if tok == nil || tok.Typ != lexer.TTComma {
					break
				}

				p.lex.NextToken()
			}

			p.expect(lexer.TTRBrace)

			expression = funcCall

		case lexer.TTLSquare:
			p.lex.NextToken()
			index := p.parseExpression()
			p.expect(lexer.TTRSquare)
			expression = &ast.ArrayIndex{
				Array: expression,
				Index: index,
			}

		default:
			opp = ToUnOp(tok.Typ)

			if opp == -1 || opp.OnLeftSide() {
				break loop
			}

			p.lex.NextToken()
			var err error
			expression, err = newUnaryOpNode(expression, opp, tok.Loc)

			if err != nil {
				panic(expression.Loc().String() + err.Error())
			}
		}
	}

	return expression
}

func (p *Parser) parsePrimary() ast.Expression {
	tok := p.lex.PeekToken()
	if tok.Typ == lexer.TTEOF {
		panic("opperand expected")
	}

	switch tok.Typ {
	case lexer.TTIdentifier:
		if tok.Literal == "alloc" {
			return p.parseAlloc()
		}

		p.lex.NextToken()

		if !p.exists(ast.Identifier(tok.Literal)) {
			panic(tok.Loc.String() + " Undefinded name: " + tok.Literal)
		}

		variable := p.get(ast.Identifier(tok.Literal))
		if vari, ok := variable.(ast.Var); ok {
			vari.Loc_ = tok.Loc
			return vari
		} else if fun, ok := variable.(ast.Func); ok {
			fun.Loc_ = tok.Loc
			return fun
		} else {
			panic("unreachable")
		}

	case lexer.TTInt:
		p.lex.NextToken()
		val := tok.ExtraInfo.(int)
		return ast.IntLit{Value: int64(val), Loc_: tok.Loc}

	case lexer.TTFloat:
		p.lex.NextToken()
		val := tok.ExtraInfo.(float64)
		return ast.FloatLit{Value: val, Loc_: tok.Loc}

	case lexer.TTString:
		p.lex.NextToken()
		return ast.StrLit{Value: tok.ExtraInfo.(string), Litteral: tok.Literal, Loc_: tok.Loc}

	case lexer.TTChar:
		p.lex.NextToken()
		return ast.CharLit{Value: tok.ExtraInfo.(rune), Litteral: tok.Literal, Loc_: tok.Loc}

	case lexer.TTTrue:
		p.lex.NextToken()
		return ast.BoolLit{Value: true, Loc_: tok.Loc}

	case lexer.TTFalse:
		p.lex.NextToken()
		return ast.BoolLit{Value: false, Loc_: tok.Loc}

	case lexer.TTLBrace:
		p.lex.NextToken()
		result := p.parseBinary(0)

		next := p.lex.NextToken().Typ
		if next == lexer.TTRBrace {
			return result
		}

		if next == lexer.TTColon {
			typ := p.parseType()

			p.expect(lexer.TTRBrace)

			return ast.Cast{Inner: result, NewType: typ, Loc_: tok.Loc}
		}

		p.expect(lexer.TTRBrace)
	}

	panic(p.lex.NextToken().String() + " illegal token") // TODO: better error handling
}

func (p *Parser) parseAlloc() ast.Expression {
	var loc lexer.Location
	tok := p.expect(lexer.TTIdentifier)
	if tok.Literal != "alloc" {
		panic("unreachable")
	}

	p.expect(lexer.TTLBrace)

	typ := p.parseType()

	var result ast.Allocation

	if typ.Kind() == ast.KindArray {
		p.expect(lexer.TTComma)

		count := p.parseExpression()

		if (count.ReturnType().Kind() != ast.KindInt &&
			count.ReturnType().Kind() != ast.KindUint) ||
			count.ReturnType().Size() != 8 {
			panic("type mismatch")
		}

		result = ast.Allocation{Typ: typ, ElemsCount: count, Loc_: loc}
	} else {
		result = ast.Allocation{
			Typ:        ast.PointerType{Name_: "", Inner: typ},
			ElemsCount: ast.IntLit{Value: 1},
			Loc_:       loc,
		}
	}

	p.expect(lexer.TTRBrace)

	return result
}

func newUnaryOpNode(expression ast.Expression, op ast.UnaryOp, opLoc lexer.Location) (ast.Expression, error) {
	switch op {
	case ast.UOPlus: // does nothing anyways
		return expression, nil
	case ast.UONegative:
		if lit, ok := expression.(ast.IntLit); ok {
			lit.Value = -lit.Value
			return lit, nil
		} else if lit, ok := expression.(ast.FloatLit); ok {
			lit.Value = -lit.Value
			return lit, nil
		}
	}

	if op.OnLeftSide() {
		return ast.UnaryOpNode{Expression: expression, Op: op, Loc_: opLoc}, nil
	} else {
		return ast.UnaryOpNode{Expression: expression, Op: op, Loc_: expression.Loc()}, nil
	}
}

func ToUnOp(t lexer.TokenType) ast.UnaryOp {
	if ast.UOCount != 6 {
		panic("Unary opperation enum length changed")
	}

	if lexer.TTCount != 60 {
		panic("Token type enum length changed")
	}

	switch t {
	case lexer.TTPlus:
		return ast.UOPlus
	case lexer.TTDash:
		return ast.UONegative
	case lexer.TTExclam:
		return ast.UOBoolNot
	case lexer.TTTilde:
		return ast.UOBinNot
	case lexer.TTAmp:
		return ast.UORef
	case lexer.TTStar:
		return ast.UODeref
	}

	return -1
}

// ToBinOp converts the token type to it's
// binary opperation, returns -1 if it isn't a binary op
func ToBinOp(t lexer.TokenType) ast.BinaryOp {
	if ast.BOCount != 28 {
		panic("Binary opperation enum length changed")
	}

	if lexer.TTCount != 60 {
		panic("Token type enum length changed")
	}

	switch t {
	case lexer.TTAssign:
		return ast.BOAssign
	case lexer.TTPlus:
		return ast.BOPlus
	case lexer.TTDash:
		return ast.BOMinus
	case lexer.TTStar:
		return ast.BOMul
	case lexer.TTSlash:
		return ast.BODiv
	case lexer.TTPercent:
		return ast.BOMod
	case lexer.TTPlusAssign:
		return ast.BOPlusAssign
	case lexer.TTDashAssign:
		return ast.BODashAssign
	case lexer.TTStarAssign:
		return ast.BOStarAssign
	case lexer.TTSlashAssign:
		return ast.BOSlashAssign
	case lexer.TTAmp:
		return ast.BOBinAnd
	case lexer.TTBar:
		return ast.BOBinOr
	case lexer.TTCaret:
		return ast.BOBinXor
	case lexer.TTAnd:
		return ast.BOBoolAnd
	case lexer.TTOr:
		return ast.BOBoolOr
	case lexer.TTGt:
		return ast.BOGt
	case lexer.TTLt:
		return ast.BOLt
	case lexer.TTGtEq:
		return ast.BOGtEq
	case lexer.TTLtEq:
		return ast.BOLtEq
	case lexer.TTShr:
		return ast.BOShr
	case lexer.TTShl:
		return ast.BOShl
	case lexer.TTAmpAssign:
		return ast.BOAndAssign
	case lexer.TTBarAssign:
		return ast.BOOrAssign
	case lexer.TTCaretAssign:
		return ast.BOXorAssign
	case lexer.TTShrAssign:
		return ast.BOShrAssign
	case lexer.TTShlAssign:
		return ast.BOShlAssign
	case lexer.TTEqual:
		return ast.BOEquals
	case lexer.TTNotEqual:
		return ast.BONotEqual
	}

	return -1
}
