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

(** {2 The 48 isometries of the cube} *)

type t

val print : Format.formatter -> t -> unit

val compose: t -> t -> t
  (** [compose i1 i2] returns the isometry result of the composition
      of [i1] and [i2] *)

val apply: t -> w:int -> h:int -> d:int -> int * int * int -> int * int * int
  (** [apply i w h d (x,y,z)] applies isometry [i]
      to the point [x, y, z] *)

val trans_size : t -> int * int * int -> int * int * int
  (** given a cuboid of size (w,h,d), [trans_size iso (w,h,d)] returns the size
      (w',h',d') of the result cuboid by [iso] *)

val all: t list
  (** all isometries (there are 48 o them) *)

val is_positive: t -> bool

val positive: t list
  (** the 24 positive isometries *)
