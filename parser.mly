
%{
  open Ast
%}

%token TILE PROBLEM CONSTANT FALSE TRUE PATTERN
%token EQUAL LSBRA RSBRA RPAR LPAR COMMA
%token <string> IDENT
%token <int * int> DIM
%token <bool array array> ASCII
%token EOF

%start <Ast.file> file

%%
file:
  l = list(decl); EOF { l }
;

decl:
| TILE;    id = IDENT; EQUAL; e = expr { Tile (id, e) }
| PROBLEM; id = IDENT; EQUAL; e = expr; tl = tile_list { Problem (id, e, tl) }
;

tile_list:
| LSBRA; l = separated_list(COMMA, expr); RSBRA { l }
;

expr:
| LPAR; e = expr; RPAR { e }
| id = IDENT
    { Var id }
| LPAR; CONSTANT; d = DIM; b = bool; RPAR
    { let w,h = d in Pattern (Array.make h (Array.make w b)) }
| CONSTANT; d = DIM; b = bool
    { let w,h = d in Pattern (Array.make h (Array.make w b)) }
| PATTERN; d = DIM; a = ASCII
    { ignore (d); (* TODO: adapter a à la dimension d *) Pattern a }
;

bool:
| FALSE { false }
| TRUE  { true  }
;


