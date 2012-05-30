exception Error

type token = 
  | XOR
  | UNION
  | TRUE
  | SYM
  | SHIFT
  | SET
  | RSBRA
  | RPAR
  | RESIZE
  | PROBLEM
  | PATTERN
  | ONE
  | MINUS
  | LSBRA
  | LPAR
  | INTER
  | IDENT of (string)
  | HAT
  | FALSE
  | EQUAL
  | EOF
  | DIM of (int * int)
  | DIFF
  | CROP
  | CONSTANT
  | COMMA
  | BARBAR
  | ASCII of (bool array array)
  | AMPAMP


val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.file)