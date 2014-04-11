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

type dim = int * int

type binop = Union | Inter | Diff | Xor

type algo_emc = Dlx | Zdd | Sat of string

type file = string

type state = On | Off

type output = Svg of file | Ascii

type setop = Shift | SetXY of bool | Resize | Crop of dim
type problem_command =
  | CountEMC of algo_emc
  | SolveEMC of algo_emc * output
  | Count    of string
  | Solve    of string * output
  | Print

type compop = Equal

type pos = Lexing.position * Lexing.position


type expr = {
  expr_node : expr_node;
  expr_pos : pos;
}
and expr_node =
  | Constant of bool array array
  | Var of string
  (* other operations: union, diff, rotations, etc. *)
  | Binary of binop * expr * expr
  | SetOp of setop * dim * expr
  | Apply of D4.t * expr

type bool_expr =
  | Boolean of bool
  | Comparison of compop * expr * expr

type tile = expr * Tiling.Tile.symmetries * Tiling.Tile.multiplicity

type tiles =
  | Tiles_id of string
  | Tiles_list of tile list

type problem = string * expr * tiles

type decl = {
  decl_node : decl_node;
  decl_pos : pos;
}

and decl_node =
  | Pattern of string * expr
  | Tiles of string * tile list
  | Problem of problem
  | Assert of bool_expr
  | Command of problem_command * string
  | Dimacs of string * string
  | Debug of state
  | Timing of state
  | Quit
  | Include of string
  | H2g2


type queue = decl list
