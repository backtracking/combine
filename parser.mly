
%{
  open Ast
  open Tiling.Tile
  open Tiling.Iso
  type option =
    | M of multiplicity
    | S of symetries

%}

%token PATTERN TILES PROBLEM CONSTANT FALSE TRUE ASSERT
%token SET SHIFT CROP RESIZE APPLY
%token MINUS AMPAMP BARBAR HAT DIFF UNION XOR INTER ONE MAYBE SYM ROT
%token EQUAL LSBRA RSBRA RPAR LPAR COMMA
%token ID ROT90 ROT180 ROT270 VERTREFL HORIZREFL DIAG1REFL DIAG2REFL
%token ASCII_OUT SVG_OUT PRINT
%token SOLVE COUNT DEBUG TIMING ON OFF DLX ZDD
%token <string> IDENT
%token <int * int> DIM
%token <bool array array> ASCII
%token EOF

%nonassoc UNION INTER DIFF XOR prec_crop
%left AMPAMP MINUS BARBAR HAT

%start <Ast.file> file

%%
file:
  l = list(decl); EOF { l }
;

decl:
| PATTERN; id = IDENT; EQUAL; e = expr
    {{decl_pos = ($startpos, $endpos);
      decl_node = Pattern (id, e)}}
| TILES; id = IDENT; EQUAL; l = tile_list
    {{decl_pos = ($startpos, $endpos);
      decl_node = Tiles (id, l)}}
| PROBLEM; id = IDENT; EQUAL; e = expr; tl = tiles
    {{decl_pos = ($startpos, $endpos);
      decl_node = Problem (id, e, tl)}}
| ASSERT; b = boolean_expr 
    {{decl_pos = ($startpos, $endpos);
      decl_node = Assert b}}
| PRINT; id = IDENT {{decl_pos = ($startpos, $endpos);
      decl_node = Command (Print, id)}}
| SOLVE; a = algo; id = IDENT {{decl_pos = ($startpos, $endpos);
      decl_node = Command (Solve a, id)}}
| COUNT; a = algo; id = IDENT {{decl_pos = ($startpos, $endpos);
      decl_node = Command (Count a, id)}}
;

algo:
| DLX { Dlx }
| ZDD { Zdd }

state:
| ON { On }
| OFF { Off }

option:
| ONE   { M Mone }
| MAYBE { M Mmaybe }
| SYM   { S Sall }
| ROT   { S Srotations }

tiles:
| l = tile_list { Tiles_list l }
| id = IDENT    { Tiles_id  id }
;

tile_list:
| LSBRA; l = separated_list(COMMA, tile); RSBRA { l }
;

isometry:
| ID { Id }
| ROT90 { Rot90 }
| ROT180 { Rot180 }
| ROT270 { Rot270 }
| VERTREFL { VertRefl }
| HORIZREFL { HorizRefl }
| DIAG1REFL { Diag1Refl }
| DIAG2REFL { Diag2Refl }
;

tile:
| e = expr; o = list(option)
    { let option (s, m) = function
        | M m' -> s, m' (* FIXME: fail on ambiguity *)
	      | S s' -> s', m (* idem *)
        in
        let s,m = List.fold_left option (Snone, Minf) o in e,s,m }
;



expr:
| LPAR; e = expr; RPAR { e }
| id = IDENT
    {{expr_pos = ($startpos, $endpos);
      expr_node = Var id}}
| CONSTANT; d = DIM; b = bool
  { let w,h = d in 
      {expr_pos = ($startpos, $endpos);
        expr_node = Constant (Array.make h (Array.make w b))} }
| UNION; e1 = expr; e2 = expr
      {{expr_pos = ($startpos, $endpos);
         expr_node = Binary (Union, e1, e2)}}
| INTER; e1 = expr; e2 = expr
      {{expr_pos = ($startpos, $endpos);
         expr_node = Binary (Inter, e1, e2)}}
| DIFF; e1 = expr; e2 = expr
      {{expr_pos = ($startpos, $endpos);
         expr_node = Binary (Diff, e1, e2)}}
| XOR; e1 = expr; e2 = expr
      {{expr_pos = ($startpos, $endpos);
         expr_node = Binary (Xor, e1, e2)}}
| e1 = expr; MINUS ;e2 = expr
      {{expr_pos = ($startpos, $endpos);
         expr_node = Binary (Diff, e1, e2)}}
| e1 = expr; AMPAMP ;e2 = expr
      {{expr_pos = ($startpos, $endpos);
         expr_node = Binary (Inter, e1, e2)}}
| e1 = expr; BARBAR ;e2 = expr
      {{expr_pos = ($startpos, $endpos);
         expr_node = Binary (Union, e1, e2)}}
| e1 = expr; HAT ;e2 = expr
      {{expr_pos = ($startpos, $endpos);
         expr_node = Binary (Xor, e1, e2)}}
| SET; e = expr; d = DIM; b = bool
    {{expr_pos = ($startpos, $endpos); 
       expr_node = SetOp (SetXY (b), d, e)} }
| CROP; pos = DIM ;d = DIM; e = expr; %prec prec_crop
    {{expr_pos = ($startpos, $endpos); 
    expr_node = SetOp (Crop(pos), d, e)}}
| SHIFT; e = expr; d = DIM
    {{expr_pos = ($startpos, $endpos); 
     expr_node = SetOp (Shift, d, e)}}
| RESIZE; e = expr; d = DIM
    {{expr_pos = ($startpos, $endpos); 
     expr_node = SetOp (Resize, d, e)}}
| APPLY; iso = isometry; e = expr
    {{expr_pos = ($startpos, $endpos); 
       expr_node = Apply (iso, e)}}
| a = ASCII
    {{expr_pos = ($startpos, $endpos); 
       expr_node = Constant a}}
;

bool:
| FALSE { false }
| TRUE  { true  }
;

boolean_expr:
| b = bool { Boolean b }
| e1 = expr; EQUAL; e2 = expr { Comparison (Equal, e1, e2)}
;


