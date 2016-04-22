exception Error

type token = 
  | ZDD
  | XOR
  | VERTREFL
  | UNION
  | TRUE
  | TIMING
  | TILES3
  | TILES
  | SYM
  | SVG_OUT
  | STRING of (string)
  | SOLVE3
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
  | PROBLEM3
  | PROBLEM
  | PRINT
  | PATTERN3
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
  | DIMACS
  | DIM of (int * int)
  | DIFF
  | DIAG2REFL
  | DIAG1REFL
  | DEBUG
  | CROP
  | COUNT3
  | COUNT
  | CONSTANT
  | COMMA
  | BARBAR
  | ASSERT
  | ASCII_OUT
  | ASCII of (bool array array)
  | APPLY
  | AMPAMP
  | ALL3
  | ALL


val queue: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.queue)