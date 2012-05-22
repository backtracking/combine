exception Error

type token = 
  | TRUE
  | TILE
  | RSBRA
  | RPAR
  | PROBLEM
  | PATTERN
  | LSBRA
  | LPAR
  | IDENT of (string)
  | FALSE
  | EQUAL
  | EOF
  | DIM of (int * int)
  | CONSTANT
  | COMMA
  | ASCII of (bool array array)


val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.file)