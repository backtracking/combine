(**************************************************************************)
(*                                                                        *)
(*  Combine - an OCaml library for combinatorics                          *)
(*                                                                        *)
(*  Copyright (C) 2012-2014                                               *)
(*    Remy El Sibaie                                                      *)
(*    Jean-Christophe Filliatre                                           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

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
  | DIMACS
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