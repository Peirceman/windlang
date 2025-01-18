package bytecode

type Opcode int8

const (
	noop Opcode = iota
	push
	aloc
	stor
	load
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
	jpne
	jple
	jplt
	prti
	prtu
	prtf
	prts
	isgt
	isge
	iseq
	isne
	isle
	islt
	over
	swap
	rote
	sgne
	band
	borr
	bnot
	bxor
	bshl
	bsrs
	bsru
	call
	rett
	exit
	negs
	negf
	base
	free
	farg
	sptr
	cvtf
	debg

	jptr = jpne
	jpfl = jpeq
)

type Instruction struct {
	Code Opcode
	Size int8
	Args interface{}
}
