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

(* Isometries of the cube; see for instance
   http://www.math.caltech.edu/~2013-14/1term/ma006a/rotcube.pdf *)

type t = int array array (* 3x3 matrix *)

let compose m1 m2 = (* matrix multiplication *)
  let mul i j = m1.(i).(0) * m2.(0).(j) + m1.(i).(1) * m2.(1).(j)
    + m1.(i).(2) * m2.(2).(j) in
  Array.init 3 (fun i -> Array.init 3 (mul i))

(* -x = w - 1 - x
   -y = h - 1 - y
   -z = d - 1 - z *)

let apply m ~w ~h ~d (x,y,z) =
  let mulx c = if c = 0 then 0 else if c = 1 then x else w - 1 - x in
  let muly c = if c = 0 then 0 else if c = 1 then y else h - 1 - y in
  let mulz c = if c = 0 then 0 else if c = 1 then z else d - 1 - z in
  let mul r = mulx r.(0) + muly r.(1) + mulz r.(2) in
  mul m.(0), mul m.(1), mul m.(2)

let trans_size m (w,h,d) =
  let mulx c = if c = 0 then 0 else w in
  let muly c = if c = 0 then 0 else h in
  let mulz c = if c = 0 then 0 else d in
  let mul r = mulx r.(0) + muly r.(1) + mulz r.(2) in
  mul m.(0), mul m.(1), mul m.(2)

let all =
  let m = Array.create_matrix 3 3 0 in
  let res = ref [] in
  let rec make = function
    | [] ->
        res := Array.map Array.copy m :: !res
    | (i,j) :: l ->
        m.(i).(j) <- 1; make l; m.(i).(j) <- -1; make l; m.(i).(j) <- 0 in
  List.iter make
    [[0,0; 1,1; 2,2];
     [0,0; 1,2; 2,1];
     [0,1; 1,0; 2,2];
     [0,1; 1,2; 2,0];
     [0,2; 1,0; 2,1];
     [0,2; 1,1; 2,0];
    ];
  !res

let () = assert (List.length all = 48)

let is_positive m =
  let p = ref 1 in
  let mul c = if c <> 0 then p := c * !p in
  Array.iter (Array.iter mul) m;
  !p = 1

let positive = List.filter is_positive all

let () = assert (List.length positive = 24)

open Format

let print fmt m =
  fprintf fmt "@[";
  let row r = Array.iter (fun x -> fprintf fmt "%2d " x) r; fprintf fmt "@\n" in
  Array.iter row m;
  fprintf fmt "@]"

