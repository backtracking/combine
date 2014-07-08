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

(* Solving the N-queens using DLX and ZDD.

The encoding of the N-queens problem to EMC is described in
Donald E. Knuth's paper "Dancing Links"

The EMC matrix has:
- N columns for the rows
- N columns for the columns
- 2N-1 columns for the left-right diagonals
- 2N-1 columns for the right-left diagonals

Each row in the EMC matrix corresponds to one way to place a queen.
Thus it has four ones, one for the column, one for the row, and one for each
diagonal.

Since some of the diagonals are not covered, only the first 2N columns
(rows and columns of the chessboard) are primary columns.

*)

open Format
open Combine

(* EMC size is N^2 * (6N-2)
   columns are the following
*)

let range = ref 0
let svg_file = ref ""

let msg = "usage: ./queens -n value"
let spec = ["-n", Arg.Set_int range,
              "  Range of the N-queens problem";
            "--svg", Arg.Set_string svg_file,
              "<file>  Output one solution in SVG format";]

let () = Arg.parse spec (fun _ ->()) msg
let () = if !range = 0 then exit 0
let n = !range
let svg_file = !svg_file

let primary = 2 * n

let row i j =
  let f k = k = i || k = n + j || k = 2*n + i + j || k = 4*n-1 + n-1-i + j in
  Array.init (6 * n - 2) f

let emc =
  let lr = ref [] in
  for i = 0 to n - 1 do for j = 0 to n - 1 do lr := row i j :: !lr done done;
  Array.of_list !lr

(* List.iter (decode sudoku emc_array) s; *)

let decode board emc_array i =
  if i < Array.length emc_array - 1 then begin
    let c = ref 0 in
    let l = ref 0 in
    for j = 0 to 2 * n - 1 do
      if emc_array.(i).(j) then begin
        if j < n then begin
          l := j;
        end
        else
          c := j - n;
      end
    done;
    board.(!l).(!c) <- true
  end

let print_board_svg u fmt =
  for i = 0 to n do
    fprintf fmt "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" \
style=\"stroke:black;stroke-width:1;\" />@\n"
      0 (i * u) (u * n) (i * u);
    fprintf fmt "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" \
style=\"stroke:black;stroke-width:1;\" />@\n"
      (i * u) 0 (i * u) (u * n);
  done

let print_cross_svg x y u fmt =
  fprintf fmt
    "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" \
style=\"stroke:#1F56D2;\"/>"
    (x * u) (y * u) ((x + 1)* u) ((y + 1) * u);
  fprintf fmt
    "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" \
style=\"stroke:#1F56D2;\"/>"
    ((x + 1) * u) (y * u) (x * u) ((y + 1) * u)

let print_solution_to_svg fmt ~width ~height n board =
  let u = 100 in
  fprintf fmt
"<?xml version=\"1.0\" standalone=\"no\"?> @\n\
@[<hov 2><svg xmlns=\"http://www.w3.org/2000/svg\" \
width=\"%d\" height=\"%d\">@\n"
  width height;
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if board.(j).(i) then
        print_cross_svg i j u fmt
    done
  done;
  print_board_svg u fmt;
  fprintf fmt "@]@\n</svg>"

let print_solution_to_svg_file f ~width ~height n board =
  let c = open_out f in
  let fmt = formatter_of_out_channel c in
  print_solution_to_svg fmt ~width ~height n board;
  fprintf fmt "@.";
  close_out c

let width = 100 * n + 1
let height = width

let time () = (Unix.times()).Unix.tms_utime

let () =
  printf "Solving the %d-queens@." n;
  printf "EMC matrix is %a@." Emc.print_matrix_size emc;
  let p = Emc.D.create ~primary emc in
  let board = Array.make_matrix n n false in
  if svg_file <> "" then begin
    let solution = Emc.D.find_solution p in
    List.iter (decode board emc) solution;
    printf "%a@." Emc.print_boolean_matrix board;
    print_solution_to_svg_file svg_file ~width ~height n board
  end else begin
    let t = time () in
    let s =  Emc.D.count_solutions p in
    printf "DLX: %d solutions, in %2.2fs@." s (time () -. t);
    let t = time () in
    let p = Emc.Z.create ~primary emc in
    printf "ZDD of size %d@." (Zdd.size p);
    let s = Emc.Z.count_solutions p in
    printf "ZDD: %d solutions, in %2.2fs@." s (time () -. t)
  end

let () =
  let s = Gc.stat () in
  let m = float (Sys.word_size / 8) *. float s.Gc.top_heap_words in
  let m = m /. 1024. /. 1024. in
  printf "Memory max: %.2f Mb@." m

let () =
  let mi, pr, ma = Gc.counters () in
  let m = float (Sys.word_size / 8) *. (mi +. ma -. pr) in
  let m = m /. 1024. /. 1024. in
  printf "Memory used: %.2f Mb@." m
