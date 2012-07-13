exception Error

type token = 
  | ZDD
  | XOR
  | VERTREFL
  | UNION
  | TRUE
  | TIMING
  | TILES
  | SYM
  | SVG_OUT
  | STRING of (string)
  | SOLVE
  | SHIFT
  | SET
  | SAT
  | RSBRA
  | RPAR
  | ROT90
  | ROT270
  | ROT180
  | ROT
  | RESIZE
  | PROBLEM
  | PRINT
  | PATTERN
  | ONE
  | ON
  | OFF
  | MINUS
  | MAYBE
  | LSBRA
  | LPAR
  | INTER
  | INCLUDE
  | IDENT of (string)
  | ID
  | HORIZREFL
  | HAT
  | H2G2
  | FALSE
  | EXIT
  | EQUAL
  | EOF
  | DLX
  | DIM of (int * int)
  | DIFF
  | DIAG2REFL
  | DIAG1REFL
  | DEBUG
  | CROP
  | COUNT
  | CONSTANT
  | COMMA
  | BARBAR
  | ASSERT
  | ASCII_OUT
  | ASCII of (bool array array)
  | APPLY
  | AMPAMP


val queue: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.queue)