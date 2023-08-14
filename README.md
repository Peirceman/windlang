# my programming language

## a language I am creating for fun

## Todo:
 - [ ] testing:  
    use testdata dir with files for files to parse and expected output
 - [ ] checking return type of expressions
 - [ ] codegen
 - [ ] fix the rest of this readme
 - [ ] add type modifiers: array, pointer
 - [ ] Use Func struct in Funccall

# spec:

### keywords
```
const var    fn        return
type  struct interface
if    elif   else
```

### letters & digits

name | value
-|-
letter | unicode_letter \| "_"
|
binary_digit  | [01]
octal_digit   | [0-7]
decimal_digit | [0-9]
hex_digit     | [0-9A-Fa-f]
digit         | decimal_digit

### numbers

name | value |
-|-
binary_number | "0b" ([binary_digit][let_and_d]) +
ocatal_number | "0o" ([octal_digit][let_and_d]) +
hex_number | "0x" ([hex_digit][let_and_d]) +
decimal_number | ([decimal_digit][let_and_d]) +
number | binary_number \| octal_number \| hex_number \| decimal_number

### identifiers

identifiers are names for things like functions, variables.

name | value
-|-
identifier | [letter][let_and_d] ([letter][let_and_d] \| [digit][let_and_d] )*

### expressions

a mathamatical expression

### Types

name | value
-|-
Type | Typename
Typename | [identifier][iden]

### variables

a variable is a way to store something

name | value
-|-
variable_decleration | "var" [identifier][iden] ":" [Type](#Types) "=" [Expression][expr] ";"
setting value | [identifier][iden] = [Expression][expr] ";"

### constants

a constant is a compile-time expression which can be reused

name | value
-|-
const_decleration | "const" [identifier][iden] ":" [Type](#Types) "=" [Expression][expr] ";"

### functions

you know what a function is

name | value
-|-
function_decleration | "fn" [identifier][iden] "(" ([identifier][iden] ":" [Type](#Types)) * ")" [ ":" [Type](#Types) ] "{" \<code> "}"


[let_and_d]: #letters-%26-digits
[iden]: #identifiers
[expr]: #expressions
