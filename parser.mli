exception Error

type token = 
  | XOR
  | UNION
  | TRUE
  | TILE
  | SHIFT
  | SET
  | RSBRA
  | RPAR
  | RESIZE
  | PROBLEM
  | PREFDIFF
  | LSBRA
  | LPAR
  | INTER
  | INFDIFF
  | IDENT of (string)
  | FALSE
  | EQUAL
  | EOF
  | DIM of (int * int)
  | CROP
  | CONSTANT
  | COMMA
  | ASCII of (bool array array)


val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.file)