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
decl | 0x02 | 4 byte id | declares variable with the given id to be of the given size
popv | 0x03 | 4 byte id | pop top bytes from stack and store it into var, amount of bytes poped is min of size argument and size of var
pshv | 0x04 | 4 byte id | push value of var onto stack
adds | 0x05 | / | pop top 2 values and push result of signed integer addition
addu | 0x06 | / | pop top 2 values and push result of unsigned integer addition
addf | 0x07 | / | pop top 2 values and push result of float addition
subs | 0x08 | / | pop top 2 values and push result of signed integer subtraction (top - bottom)
subu | 0x09 | / | pop top 2 values and push result of unsigned integer subtraction (top - bottom)
subf | 0x0A | / | pop top 2 values and push result of float subtraction (top - bottom)
muls | 0x0B | / | pop top 2 values and push result of signed integer multiplication
mulu | 0x0C | / | pop top 2 values and push result of unsigned integer multiplication
mulf | 0x0D | / | pop top 2 values and push result of float multiplication
divs | 0x0E | / | pop top 2 values and push result of signed integer division (top / bottom)
divu | 0x0F | / | pop top 2 values and push result of unsigned integer division (top / bottom)
divf | 0x10 | / | pop top 2 values and push result of float division (top / bottom)
mods | 0x11 | / | pop top 2 values and push modulo of signed integer division (top / bottom)
modu | 0x12 | / | pop top 2 values and push modulo of unsigned integer division (top / bottom)
dupe | 0x13 | / | duplicates top value on the stack
pops | 0x14 | / | pops the top value from the stack and discards it
cmpu | 0x15 | / | pop top 2 values and compare as unsigned integer and push result push positive number if top > bottom, 0 if top = bottom, negeative number if top < bottom
cmps | 0x16 | / | same but unsigned, the pushed value will be a signed integer of the same size as size byte
cmpf | 0x17 | / | same but float, the pushed value will be a signed integer of the same size as size byte
jump | 0x18 | 4 byte instruction index | jump
jpgt | 0x19 | 4 byte instruction index | jump if greater, top value of the stack as signed is greater than 0
jpge | 0x1A | 4 byte instruction index | jump if greater or equal, top value as signed is non-negative
jpeq | 0x1B | 4 byte instruction index | jump if equal, top value is 0
jple | 0x1C | 4 byte instruction index | jump if less or equal, top value as signed is 0 or negative
jplt | 0x1D | 4 byte instruction index | jump if less, top value as signed is negative
prti | 0x1E | / | **temporary instruction** pop top value on the stack and print as signed integer
prtu | 0x1F | / | **temporary instruction** pop top value on the stack and print as unsigned integer
prtf | 0x20 | / | **temporary instruction** pop top value on the stack and print as float
prts | 0x21 | 4 byte id | **temporary instruction** print var as string

Todo:
- function calling
- poiner stuff
- struct indexing

### data:
section type "data"
its just many times this format:
id (4bytes) len (4bytes) data (n bytes)
len is in bytes
