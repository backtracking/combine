/**************************************************************************/
/*                                                                        */
/*  Combine - an OCaml library for combinatorics                          */
/*                                                                        */
/*  Copyright (C) 2012-2014                                               */
/*    Remy El Sibaie                                                      */
/*    Jean-Christophe Filliatre                                           */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2.1, with the special exception on linking            */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

%{
  open Ast
  open Tiling
  open D4
  type option =
    | M of multiplicity
    | S of symmetries

%}

%token PATTERN TILES PROBLEM
%token PATTERN3 TILES3 PROBLEM3
%token CONSTANT FALSE TRUE
%token SET SHIFT CROP RESIZE APPLY
%token EXIT INCLUDE ASSERT
%token MINUS AMPAMP BARBAR HAT DIFF UNION XOR INTER ONE MAYBE SYM ROT
%token EQUAL LSBRA RSBRA RPAR LPAR COMMA
%token ID ROT90 ROT180 ROT270 VERTREFL HORIZREFL DIAG1REFL DIAG2REFL
%token ASCII_OUT SVG_OUT PRINT
%token SOLVE SOLVE3 COUNT DIMACS DEBUG TIMING ON OFF DLX ZDD SAT
%token <string> IDENT
%token <string> STRING
%token <int * int> DIM
%token <bool array array> ASCII
%token H2G2
%token EOF

%nonassoc UNION INTER DIFF XOR APPLY prec_crop
%left AMPAMP MINUS BARBAR HAT

%start <Ast.queue> queue

%%
queue:
  l = list(decl); EOF { l }
;

decl:
| PATTERN; id = IDENT; EQUAL; e = expr
    {{decl_pos = ($startpos, $endpos);
      decl_node = Pattern (id, e)}}
| PATTERN3; id = IDENT; EQUAL; l = expr_list
    { { decl_pos = ($startpos, $endpos);
        decl_node = Pattern3 (id, l) } }
| TILES; id = IDENT; EQUAL; l = tile_list
    {{decl_pos = ($startpos, $endpos);
      decl_node = Tiles (id, l)}}
| TILES3; id = IDENT; EQUAL; l = tile_list
    {{decl_pos = ($startpos, $endpos);
      decl_node = Tiles3 (id, l)}}
| PROBLEM; id = IDENT; EQUAL; e = expr; tl = tiles
    {{decl_pos = ($startpos, $endpos);
      decl_node = Problem (id, e, tl)}}
| PROBLEM3; id = IDENT; EQUAL; e = expr; tl = tiles
    {{decl_pos = ($startpos, $endpos);
      decl_node = Problem3 (id, e, tl)}}
| ASSERT; b = boolean_expr
    {{decl_pos = ($startpos, $endpos);
      decl_node = Assert b}}
| PRINT; id = IDENT {{decl_pos = ($startpos, $endpos);
      decl_node = Command (Print, id)}}
| SOLVE; a = algo_emc; id = IDENT; out = output
    {{decl_pos = ($startpos, $endpos);
      decl_node = Command (SolveEMC (a, out), id)}}
| SOLVE; a = IDENT; id = IDENT; out = output
    {{decl_pos = ($startpos, $endpos);
      decl_node = Command (Solve (a, out), id)}}
| SOLVE3; a = algo_emc; id = IDENT; out = output
    {{decl_pos = ($startpos, $endpos);
      decl_node = Command3 (SolveEMC (a, out), id)}}
| COUNT; a = algo_emc; id = IDENT
    {{decl_pos = ($startpos, $endpos);
      decl_node = Command (CountEMC a, id)}}
| COUNT; a = IDENT; id = IDENT
    {{decl_pos = ($startpos, $endpos);
      decl_node = Command (Count a, id)}}
| DIMACS; id = IDENT; file = STRING
    {{decl_pos = ($startpos, $endpos); decl_node = Dimacs (id, file) }}
| DEBUG; st = state; {{decl_pos = ($startpos, $endpos);
      decl_node = Debug st}}
| TIMING; st = state; {{decl_pos = ($startpos, $endpos);
      decl_node = Timing st}}
| EXIT {{decl_pos = ($startpos, $endpos);
      decl_node = Quit}}
| INCLUDE; s = STRING {{decl_pos = ($startpos, $endpos);
      decl_node = Include s}}
| H2G2 {{decl_pos = ($startpos, $endpos);
      decl_node = H2g2}}
;

algo_emc:
| DLX             { Dlx }
| ZDD             { Zdd }
| SAT; s = STRING { Sat s }

state:
| ON  { On }
| OFF { Off }

option:
| ONE   { M Mone }
| MAYBE { M Mmaybe }
| SYM   { S Sall }
| ROT   { S Spositive }

tiles:
| l = tile_list { Tiles_list l }
| id = IDENT    { Tiles_id  id }
;

tile_list:
| LSBRA; l = separated_list(COMMA, tile); RSBRA { l }
;

output:
| SVG_OUT; s = STRING { Svg s }
| ASCII_OUT { Ascii }

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

expr_list:
| LSBRA; l = separated_list(COMMA, expr); RSBRA { l }
;

expr:
| LPAR; e = expr; RPAR
    { e }
| id = IDENT
    {{expr_pos = ($startpos, $endpos);
      expr_node = Var id}}
| CONSTANT; d = DIM; b = bool
    { let w,h = d in
      { expr_pos = ($startpos, $endpos);
        expr_node = Pattern (Array.make h (Array.make w b))} }
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
    {{ expr_pos = ($startpos, $endpos);
       expr_node = Pattern a}}
;

bool:
| FALSE { false }
| TRUE  { true  }
;

boolean_expr:
| b = bool { Boolean b }
| e1 = expr; EQUAL; e2 = expr { Comparison (Equal, e1, e2)}
;
