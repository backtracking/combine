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


type t = Id | Rot90 | Rot180 | Rot270 | VertRefl | HorizRefl |
    Diag1Refl | Diag2Refl

type iso = t

module S = Set.Make (
  struct
    type t = iso
    let compare = Pervasives.compare
  end)

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

let print fmt iso = Format.fprintf fmt "%s" (to_string iso)


type sub_group = S.t

let d4 = all

let quotient g1 g2 = 
  assert false (* TODO *)

let elements g = g

let generated_by s =
  s
    (* TODO *)







