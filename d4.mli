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

val print : Format.formatter -> t -> unit

val is_positive: t -> bool

val compose: t -> t -> t
  (* [compose i1 i2] returns the isometry result of the composition 
     of [i1] and [i2] *)
  
val to_string : t -> string

val apply : t -> w:int -> h:int -> int * int -> int * int
  (* [apply i w h x y] returns the couple result of the isometry [i]
     applied on the couple [x, y] *)

val trans_size : t -> int * int -> int * int

module S: Set.S with type elt = t

val all: S.t
  (* all elements of D4 *)

type sub_group

val quotient : sub_group -> sub_group -> sub_group
  (* computes the quotient of two subgroups of D4 *)

val d4: sub_group
  (* D4 itself *)

val elements: sub_group -> S.t

val generated_by: S.t -> sub_group





