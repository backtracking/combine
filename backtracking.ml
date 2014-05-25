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

open Format
open Tiling
open Tiling.Pattern
open Tiling.Tile
open Tiling.Problem

let debug = ref false

type algorithm = (solution -> unit) -> problem -> unit

let algorithms = Hashtbl.create 17
let add = Hashtbl.add algorithms
let find = Hashtbl.find algorithms

let all_tiles_same_size tl =
  let size = ref None in
  let check_size t = match !size with
    | None -> size := Some t.pattern.size
    | Some s -> if s <> t.pattern.size then
        invalid_arg "equal_size: tiles of different sizes" in
  List.iter check_size tl;
  match !size with None -> assert false | Some s -> s

let add_tile grid ({pattern=p}, tx, ty) =
  let grid = Matrix.copy grid in
  for y = 0 to p.height - 1 do for x = 0 to p.width - 1 do
    if p.matrix.(y).(x) then begin
      if not grid.(ty + y).(tx + x) then raise Exit;
      grid.(ty + y).(tx + x) <- false
    end
  done done;
  grid

(* check that the connected component of c has an area multiple of size *)
let check_areas grid size =
  let grid = Matrix.copy grid in
  let h = Array.length grid - 1 in
  let w = Array.length grid.(0) - 1 in
  let rec check_rec s = function
    | [] -> if s mod size <> 0 then raise Exit
    | (y, x) :: rem ->
	let s, rem = if x < w && grid.(y).(x+1) then begin
	  grid.(y).(x+1) <- false; s+1, (y, x+1)::rem end else s,rem in
	let s,rem = if y < h && grid.(y+1).(x) then begin
	  grid.(y+1).(x) <- false; s+1, (y+1, x)::rem end else s,rem in
	let s,rem = if x > 0 && grid.(y).(x-1) then begin
	  grid.(y).(x-1) <- false; s+1, (y, x-1)::rem end else s,rem in
	let s,rem = if y > 0 && grid.(y-1).(x) then begin
	  grid.(y-1).(x) <- false; s+1, (y-1, x)::rem end else s,rem in
	check_rec s rem
  in
  try
    for y = 0 to h do for x = 0 to w do
      if grid.(y).(x) then begin grid.(y).(x) <- false; check_rec 1 [(y, x)] end
    done done;
    true
  with Exit ->
    false

let rec backtracking f sol size grid todo pieces =
  if todo = 0 then f sol
  else if check_areas grid size then match pieces with
    | [] -> ()
    | tiles :: other_pieces ->
        let test_move (t, x, y as m) =
          try
            let grid = add_tile grid m in
            let sol = m :: sol in
            let todo = todo - size in
            match t.multiplicity with
              | Mone | Mmaybe -> backtracking f sol size grid todo other_pieces
              | Minf          -> backtracking f sol size grid todo pieces
          with Exit ->
            ()
        in
        List.iter test_move tiles;
        match tiles with
          | ({multiplicity = Mmaybe | Minf},_,_) :: _ ->
              backtracking f sol size grid todo other_pieces
          | [] | ({ multiplicity = Mone },_,_) :: _ -> ()

let possible_moves grid size tile =
  let res = ref [] in
  let test tile =
    for ty = 0 to grid.height - tile.pattern.height do
      for tx = 0 to grid.width - tile.pattern.width do
        let m = (tile, tx, ty) in
        try
          let grid = add_tile grid.matrix m in
          if check_areas grid size then res := m :: !res
        with Exit -> ()
      done
    done
  in
  List.iter test (tile :: create_all_symmetries tile);
  if !debug then printf "%d moves for tile %s@." (List.length !res) tile.name;
  !res

let equal_size f p =
  if p.pieces = [] then invalid_arg "equal_size: no tile given";
  let size = all_tiles_same_size p.pieces in
  if p.grid.size mod size <> 0 then
    invalid_arg "equal_size: problem size is not a multiple of tile size";
  let pieces = List.map (possible_moves p.grid size) p.pieces in
  backtracking f [] size p.grid.matrix p.grid.size pieces

let () = add "equal_size" equal_size
