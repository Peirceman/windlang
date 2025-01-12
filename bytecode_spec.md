# Bytecode format

this shit will all change
stack based language

Big-endian and all that pretty stuff

## magic bytes + version
WBC\u0000
4 byte version: 1 byte major 1 byte minor 2 byte revision

## sections:
header + data
header = 4 byte section type + 4 byte section length

### Code section:
section type: "code"
an instruction is 2 bytes followed by optional argument(s)  
the first byte is the opcode and the second byte indicates
the size of the paramater in bytes (ie. 64 bit vs 32 bit number)
the size is expected to be one of 1,2,4,8


here is the list of all instructions, n is specified by the size byte

instruction | opcode | arguments | explanation
-|-|-|-
noop | 0x00 | / | does nothing, the size byte also changes nothing
push | 0x01 | n bytes | pushes the litteral bytes, amout of bytes given by size
aloc | 0x02 | / | pops top 8 byte value from the stack, allocates that many bytes and pushes a pointer to it
stor | 0x03 | / | pop top size byte amount of bytes from stack and store it into the pointer below it
load | 0x04 | / | pushes size byte amount of bytes from pointer onto the stack
adds | 0x05 | / | pop top 2 values and push result of signed integer addition
addu | 0x06 | / | pop top 2 values and push result of unsigned integer addition
addf | 0x07 | / | pop top 2 values and push result of float addition
subs | 0x08 | / | pop top 2 values and push result of signed integer subtraction (bottom - top)
subu | 0x09 | / | pop top 2 values and push result of unsigned integer subtraction (bottom - top)
subf | 0x0A | / | pop top 2 values and push result of float subtraction (bottom - top)
muls | 0x0B | / | pop top 2 values and push result of signed integer multiplication
mulu | 0x0C | / | pop top 2 values and push result of unsigned integer multiplication
mulf | 0x0D | / | pop top 2 values and push result of float multiplication
divs | 0x0E | / | pop top 2 values and push result of signed integer division (bottom / top)
divu | 0x0F | / | pop top 2 values and push result of unsigned integer division (bottom / top)
divf | 0x10 | / | pop top 2 values and push result of float division (bottom / top)
mods | 0x11 | / | pop top 2 values and push modulo of signed integer division (bottom / top)
modu | 0x12 | / | pop top 2 values and push modulo of unsigned integer division (bottom / top)
dupe | 0x13 | / | duplicates top value on the stack
pops | 0x14 | / | pops the top value from the stack and discards it
cmpu | 0x15 | / | pop top 2 values and compare as unsigned integer and push result push positive number if top > bottom, 0 if top = bottom, negeative number if top < bottom
cmps | 0x16 | / | same but unsigned, the pushed value will be a signed integer of the same size as size byte
cmpf | 0x17 | / | same but float, the pushed value will be a signed integer of the same size as size byte
jump | 0x18 | 4 byte instruction index | jump
jpgt | 0x19 | 4 byte instruction index | jump if greater, top value of the stack as signed is greater than 0
jpge | 0x1A | 4 byte instruction index | jump if greater or equal, top value as signed is non-negative
jpeq/jpfl | 0x1B | 4 byte instruction index | jump if equal / jump if false, top value is 0
jpne/jptr | 0x1C | 4 byte instruction index | jump if not equal / jump if true, top value is not 0
jple | 0x1D | 4 byte instruction index | jump if less or equal, top value as signed is 0 or negative
jplt | 0x1E | 4 byte instruction index | jump if less, top value as signed is negative
prti | 0x1F | / | **temporary instruction** pop top value on the stack and print as signed integer
prtu | 0x20 | / | **temporary instruction** pop top value on the stack and print as unsigned integer
prtf | 0x21 | / | **temporary instruction** pop top value on the stack and print as float
prts | 0x22 | / | **temporary instruction** print top pointer on stack as string(8 byte size followed by actual string)
isgt | 0x23 | / | chekcs if the result of cmp is greater (positive) and pushes 1 if true, 0 if false
isge | 0x24 | / | chekcs if the result of cmp is greater or equal (0 or positive) and pushes 1 if true, 0 if false
iseq | 0x25 | / | chekcs if the result of cmp is equal (0) and pushes 1 if true, 0 if false
isne | 0x26 | / | chekcs if the result of cmp is not equal (not 0) and pushes 1 if true, 0 if false
isle | 0x27 | / | chekcs if the result of cmp is less or equal (0 or negative) and pushes 1 if true, 0 if false
islt | 0x28 | / | chekcs if the result of cmp is less (negative) and pushes 1 if true, 0 if false
over | 0x29 | / | copies the second value on the stack to the top
swap | 0x2A | / | swaps the top two values on the stack
rote | 0x2B | / | moves third value to second place, second value to top and top value to third c b a -> a c b
sgne | 0x2C | / | extends top signed integer to be one size larger: 8 bits becomes 16, 16 32 and 32 64 janky but works i guess, size byte is input size
band | 0x2D | / | performs a binary and on the top two values
borr | 0x2E | / | performs a binary or on the top two values
bnot | 0x2F | / | performs a binary not on the top value
bxor | 0x30 | / | performs a binary xor on the top two values
bshl | 0x31 | / | performs a binary left shift the second value on the stack, a top value amount of times
bsrs | 0x32 | / | performs a signed binary right shift the second value on the stack, a top value amount of times
bsru | 0x33 | / | performs an unsigned binary right shift the second value on the stack, a top value amount of times
call | 0x34 | 4 byte instruction index | pushes the next instruction's index onto the callstack and jumps to given adress
rett | 0x35 | / | pops top index from the callstack and jumps there
exit | 0x36 | / | exits the program with top value as return value
negs | 0x37 | / | arithmetic not / negate on signed integer
negf | 0x38 | / | arithmetic not / negate on float
base | 0x39 | / | pushes the stack base pointer
free | 0x3A | / | frees the allocated pointer on the top of the stack
farg | 0x3B | / | indicates that everything from now on until the next `call` instruction is a function argument
sptr | 0x3C | / | pushes a pointer to the top of the stack

`over` `swap` and `rote` assume all elements are of same size because they probably are, otherwise
what are you doing. These instructions were inspired by porth: [https://gitlab.com/tsoding/porth/-/blob/master/porth.porth?ref_type=heads]

Todo:
- function calling
- poiner stuff
- struct indexing

#### Function calling semantics

the arguments must be from top to bottom the same as from left to right
f(a, b, c) needs the stack to be the following (from bottom to top):
c b a (a pushed last, c first)

space for the return value must be allocated by the caller and a pointer
to that location must be given as the first argument

everything above the base pointer will be cleared after returning

#### stack base pointer
points to the base of the stack, aka where the top of the stack was when the
function was called, to insure arguments are on top of the base pointer, use the
instruction `farg` to instruct that everything afterwards will be function arguments

### data:
just data

## pointers:
pointers are 8 byte values.
It doesn't have to map to a real memory adress and an interpreter can define them
however it wants, but increasing a pointer does have to return the next value
(if it's valid) so arrays can work
