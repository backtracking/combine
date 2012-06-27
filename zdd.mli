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

type unique = int
type zdd = private Bottom | Top | Node of unique * int * zdd * zdd

module S : Set.S with type elt = int
include Set.S with type t = zdd and type elt = S.t

val bottom: t
val top: t
val construct: int -> t -> t -> t

val choose_list: t -> int list
val iter_list: (int list -> unit) -> t -> unit

val unique : t -> int

module type ARITH = sig
  type t
  val zero: t
  val one: t
  val add: t -> t -> t
end
  
module Cardinal(A: ARITH) : sig val cardinal: t -> A.t end

val size: t -> int
  (** Number of internal nodes of a given ZDD.
      Each node occupies 5 words, so the total space used by a ZDD [z]
      is [size z * 5 * Sys.word_size lsl 3] bytes. *)

val print_to_dot: Format.formatter -> t -> unit
  (** [print_to_dot fmt z] write a string on dot format corresponding 
   to the drawing of the Zdd [z] *)

val print_to_dot_file: string -> t -> unit
  (** [print_to_dot_file f z] write a string on dot format corresponding 
   to the drawing of the Zdd [z] in the given file [f]*)


val print_stat: Pervasives.out_channel -> unit
