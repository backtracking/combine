(**************************************************************************)
(*                                                                        *)
(*  Combine - an OCaml library for combinatorics                          *)
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


type t = Id | Rot90 | Rot180 | Rot270 | VertRefl | HorizRefl |
    Diag1Refl | Diag2Refl

type iso = t

let compose i1 i2 = match i1, i2 with
  | Id, a | a, Id -> a
    (* rotations *)
  | Rot90, Rot270
  | Rot270, Rot90
  | Rot180, Rot180 -> Id
  | Rot180, Rot270
  | Rot270, Rot180 -> Rot90
  | Rot90, Rot90
  | Rot270, Rot270 -> Rot180
  | Rot180, Rot90
  | Rot90, Rot180 -> Rot270
    (* reflections *)
  | VertRefl, VertRefl
  | HorizRefl, HorizRefl
  | Diag1Refl, Diag1Refl
  | Diag2Refl, Diag2Refl -> Id
  | VertRefl, Diag2Refl
  | Diag2Refl, HorizRefl
  | HorizRefl, Diag1Refl
  | Diag1Refl, VertRefl -> Rot90
  | VertRefl, HorizRefl
  | HorizRefl, VertRefl
  | Diag2Refl, Diag1Refl
  | Diag1Refl, Diag2Refl -> Rot180
  | HorizRefl, Diag2Refl
  | Diag2Refl, VertRefl
  | VertRefl, Diag1Refl
  | Diag1Refl, HorizRefl -> Rot270
    (* rotation/reflection *)
  | Rot90, VertRefl
  | Rot270, HorizRefl
  | Rot180, Diag2Refl
  | VertRefl, Rot270
  | HorizRefl, Rot90
  | Diag2Refl, Rot180 -> Diag1Refl
  | Rot90, HorizRefl
  | Rot180, Diag1Refl
  | Rot270, VertRefl
  | VertRefl, Rot90
  | HorizRefl, Rot270
  | Diag1Refl, Rot180 -> Diag2Refl
  | Rot90, Diag1Refl
  | Rot180, VertRefl
  | Rot270, Diag2Refl
  | VertRefl, Rot180
  | Diag1Refl, Rot270
  | Diag2Refl, Rot90 -> HorizRefl
  | Rot90, Diag2Refl
  | Rot180, HorizRefl
  | Rot270, Diag1Refl
  | HorizRefl, Rot180
  | Diag1Refl, Rot90
  | Diag2Refl, Rot270 -> VertRefl


let to_string = function
  | Id -> "Id"
  | HorizRefl -> "HorizRefl"
  | VertRefl -> "VertRefl"
  | Rot180 -> "Rot180"
  | Rot270 -> "Rot270"
  | Rot90 -> "Rot90"
  | Diag1Refl -> "Diag1Refl"
  | Diag2Refl -> "Diag2Refl"

let print fmt iso = Format.fprintf fmt "%s" (to_string iso)

module S = Set.Make (
  struct
    type t = iso
    let compare = Pervasives.compare
  end)

let all =
  ( S.add Id
      ( S.add Rot90
	  ( S.add Rot270
	      ( S.add Rot180
		  ( S.add VertRefl
		      ( S.add HorizRefl
			  ( S.add Diag2Refl
			      ( S.add Diag1Refl S.empty))))))))

let is_positive = function
  | Id | Rot90 | Rot180 | Rot270 -> true
  | VertRefl | HorizRefl | Diag1Refl | Diag2Refl -> false

(* sanity checks for compose *)

(* 1. positivity rules *)
let () =
  S.iter (fun a ->
    S.iter (fun b -> assert (
      is_positive (compose a b) = (is_positive a = is_positive b))
    ) all ) all

(* 2. compose is associative *)
let () =
  S.iter (
    fun a ->
      S.iter (
        fun b ->
          S.iter (
            fun c ->
              assert (compose a (compose b c) = compose (compose a b) c)) all
      ) all ) all

let apply = function
  | Id -> (fun ~w ~h p -> p)
  | Rot180 -> (fun ~w ~h (x, y) -> (w - 1 - x , h - 1 - y))
  | HorizRefl -> (fun ~w ~h (x, y) -> (x, h - 1 - y))
  | VertRefl -> (fun ~w ~h (x, y) -> (w - 1- x, y))
  | Rot90 -> (fun ~w ~h (x, y) -> (h - 1 - y, x))
  | Rot270 -> (fun ~w ~h (x, y) -> (y, w - 1 - x))
  | Diag1Refl -> (fun ~w ~h (x, y) -> (y, x))
  | Diag2Refl -> (fun ~w ~h (x, y) -> (h - 1 - y, w - 1 - x))

let trans_size iso (h, w) = match iso with
  | Id
  | HorizRefl
  | VertRefl
  | Rot180 -> (h, w)
  | Rot270
  | Rot90
  | Diag1Refl
  | Diag2Refl -> (w, h)

type subgroup = S.t

(* the 10 subgroups of D4 *)
(* 1 element *)
let id = S.singleton Id
(* 2 elements *)
let vert = S.union id (S.singleton VertRefl)
let horiz = S.union id (S.singleton HorizRefl)
let rot180 = S.union id (S.singleton Rot180)
let diag1 = S.union id (S.singleton Diag1Refl)
let diag2 = S.union id (S.singleton Diag2Refl)
(* 4 elements *)
let positive = S.add Rot180 (S.add Rot90 (S.add Rot270 id))
let refl_hv = S.add Rot180 (S.add VertRefl (S.add HorizRefl id))
let refl_12 = S.add Rot180 (S.add Diag1Refl (S.add Diag2Refl id))
(* 8 elements = D4 itself *)
let d4 = all

let subgroup s =
  (* TODO: check that [s] is indeed a subgroup;
     otherwise fail or saturate it *)
  s

type group =
  | Id | Vert | Horiz | Rot180 | Diag1 | Diag2 | Positive
  | Refl_hv | Refl_12 | D4

let quotient g1 g2 =
  if S.equal g2 id then g1
  else if S.equal g1 g2 then id
  else if S.equal g1 refl_hv then
    if S.equal g2 vert then horiz
    else if S.equal g2 horiz then vert
    else if S.equal g2 rot180 then refl_hv
    else invalid_arg "quotient"
  else if S.equal g1 positive then
    if S.equal g2 rot180 then positive
    else invalid_arg "quotient"
  else if S.equal g1 refl_12 then
    if S.equal g2 diag1 then diag2
    else if S.equal g2 diag2 then diag1
    else if S.equal g2 rot180 then refl_12
    else invalid_arg "quotient"
  else if S.equal g1 d4 then
    if S.equal g2 refl_hv then positive
    else if S.equal g2 positive then d4
    else if S.equal g2 refl_12 then positive
    else if S.equal g2 vert then positive
    else if S.equal g2 horiz then positive
    else if S.equal g2 rot180 then d4
    else if S.equal g2 diag2 then positive
    else if S.equal g2 diag1 then positive
    else assert false
  else
    invalid_arg "quotient"

let elements g = g

let compl = function
  | Id -> D4
  | Vert -> Positive
  | Horiz -> Positive
  | Rot180 -> Refl_12
  | Diag1 -> Refl_hv
  | Diag2 -> Refl_hv
  | Positive -> Vert
  | Refl_hv -> Diag1
  | Refl_12 -> Horiz
  | D4 -> Id

let generated_by s =
  s
    (* TODO *)







