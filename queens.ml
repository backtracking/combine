(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2012                                                    *)
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

(* Queens Module *)

open Format
open Reml

(* emc size : rang * (rang + 1) *)

let range = ref 0
let svg_file = ref ""

let msg = "usage: ./queens -n value"
let spec = ["-n", Arg.Set_int range,
              "  Range of the N-queens problem";
            "--svg", Arg.Set_string svg_file,
              "<file>  Output one solution in SVG format";]

let () = Arg.parse spec (fun _ ->()) msg
let () = if !range = 0 then exit 0
let range = !range
let svg_file = !svg_file

let get_line i j n =
  let line = Array.make (6 * n - 2) false in
  line.(i) <- true;
  line.(n + j) <- true;
  line.(2 * n + i + j) <- true;
  line.(4 * n - 1 + n - 1 - i + j) <- true;
  line

let emc n =
  let lr = ref [] in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      lr := get_line i j n :: !lr
    done
  done;
  Array.of_list !lr

(* List.iter (decode sudoku emc_array) s; *)

let decode n board emc_array i =
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


let print_board_svg n u fmt =
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
  print_board_svg n u fmt;
  fprintf fmt "@]@\n</svg>"

let print_solution_to_svg_file f ~width ~height n board =
  let c = open_out f in
  let fmt = formatter_of_out_channel c in
  print_solution_to_svg fmt ~width ~height n board;
  fprintf fmt "@.";
  close_out c

let width = 100 * range + 1
let height = width

let () =
  printf "Solving the %d-queens@." range;
  let emc_array = emc range in
  printf "EMC matrix is %a@." Emc.print_matrix_size emc_array;
  let p = Emc.D.create ~primary:(2 * range) emc_array in
  let board = Array.make_matrix range range false in
  if svg_file <> "" then begin
    let solution = Emc.D.find_solution p in
    List.iter (decode range board emc_array) solution;
    printf "%a@." Emc.print_boolean_matrix board;
    print_solution_to_svg_file svg_file ~width ~height range board
  end else begin
    printf "DLX: %d solutions@." (Emc.D.count_solutions p);
    let p = Emc.Z.create ~primary:(2 * range) emc_array in
    printf "ZDD: %d solutions@." (Emc.Z.count_solutions p)
  end
