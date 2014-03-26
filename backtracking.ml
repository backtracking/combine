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

open Tiling
open Tiling.Pattern
open Tiling.Tile

let all_tiles_same_size tl =
  let size = ref None in
  let check_size t = match !size with
    | None -> size := Some t.pattern.size
    | Some s -> if s <> t.pattern.size then
        invalid_arg "equal_size: tiles of different sizes" in
  List.iter check_size tl;
  match !size with None -> assert false | Some s -> s

let equal_size f p =
  if p.pieces = [] then invalid_arg "equal_size: no tile given";
  let size = all_tiles_same_size p.pieces in
  if p.grid.size mod size <> 0 then
    invalid_arg "equal_size: problem size is not a multiple of tile size";
  assert false (*TODO*)
