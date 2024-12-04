package WindBytecode

type Opcode int8

const (
	noop Opcode = iota
	push
	decl
	popv
	pshv
	adds
	addu
	addf
	subs
	subu
	subf
	muls
	mulu
	mulf
	divs
	divu
	divf
	mods
	modu
	dupe
	pops
	cmpu
	cmps
	cmpf
	jump
	jpgt
	jpge
	jpeq
	jple
	jplt
	prti
	prtu
	prtf
	prts
)

type Instruction struct {
	Code Opcode
	Size int8
	Args interface{}
}
