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

(** {2 Dihedral group D4} *)

(** the 8 elements of D4 are the 8 isometries of the square *)
type t =
  | Id | Rot90 | Rot180 | Rot270
  | VertRefl | HorizRefl | Diag1Refl | Diag2Refl

val print : Format.formatter -> t -> unit

val is_positive: t -> bool

val compose: t -> t -> t
  (** [compose i1 i2] returns the isometry result of the composition
      of [i1] and [i2] *)

val to_string : t -> string

val apply : t -> w:int -> h:int -> int * int -> int * int
  (** [apply i w h x y] returns the couple result of the isometry [i]
      applied on the couple [x, y] *)

val trans_size : t -> int * int -> int * int
  (** given a rectangle of size (w,h), [trans_size iso (w,h)] returns the size
      (w',h') of the result rectangle by [iso] *)

module S: Set.S with type elt = t

val all: S.t
  (** all elements of D4 *)

type subgroup

val quotient : subgroup -> subgroup -> subgroup
  (** computes the quotient of two subgroups of D4 *)

(** {2 The 10 subgroups of D4} *)

(** 1 element *)
val id: subgroup			(* { Id } *)

(** 2 elements *)
val vert: subgroup			(* { Id, VertRefl } *)
val horiz: subgroup			(* { Id, HorizRefl } *)
val rot180: subgroup
val diag1: subgroup
val diag2: subgroup

(** 4 elements *)
val positive: subgroup
val refl_hv: subgroup
val refl_12: subgroup

(** 8 elements = D4 itself *)
val d4: subgroup

val elements: subgroup -> S.t

val subgroup: S.t -> subgroup

