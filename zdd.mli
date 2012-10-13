(**************************************************************************)
(*                                                                        *)
(*  ReML - an OCaml library for combinatorics                             *)
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

(** Zero-suppressed binary decision diagrams.
    See for instance The Art of Computer Programming, volume 4A p. 249 *)

type unique = int
type t = private Bottom | Top | Node of unique * int * t * t

val bottom: t
val top: t
val construct: int -> t -> t -> t
  (** Smart constructors for ZDD.
      [construct i z1 z2] raises [Invalid_argument] if [i] is not
      smaller than roots of [z1] and [z2]. *)

val unique: t -> int
  (** Each ZDD has a unique integer *)

(** {2 A ZDD represents a set of sets of integers.}
    Hence this module provides all operations from OCaml's [Set],
    with elements being sets of integers (see type [elt] below) *)

module S : Set.S with type elt = int
type elt = S.t
val empty : t
val is_empty : t -> bool
val mem : elt -> t -> bool
val add : elt -> t -> t
val singleton : elt -> t
val remove : elt -> t -> t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val subset : t -> t -> bool
val iter : (elt -> unit) -> t -> unit
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
val for_all : (elt -> bool) -> t -> bool
val exists : (elt -> bool) -> t -> bool
val filter : (elt -> bool) -> t -> t
val partition : (elt -> bool) -> t -> t * t
val cardinal : t -> int
val elements : t -> elt list
val min_elt : t -> elt
val max_elt : t -> elt
val choose : t -> elt
val split : elt -> t -> t * bool * t

val choose_list: t -> int list
val iter_list: (int list -> unit) -> t -> unit

(** {2 Cardinal using big integers}
    Function [cardinal] above may easily overflow. Therefore, we also
    provide a functor to compute the cardinal of a ZDD using any arithmetic
    (e.g. Num, Gmp, Zarith, etc.) *)

module type ARITH = sig
  type t
  val zero: t
  val one: t
  val add: t -> t -> t
end

module Cardinal(A: ARITH) : sig val cardinal: t -> A.t end

(** {2 Other functions} *)

val size: t -> int
  (** Number of internal nodes of a given ZDD.
      Each node occupies 5 words, so the total space used by a ZDD [z]
      is [size z * 5 * Sys.word_size/8] bytes. *)

val print: Format.formatter -> t -> unit
  (** Prints a ZDD as a set of sets of integers, e.g. [{ {0}, {1}, {1,2} }]. *)

val print_to_dot: Format.formatter -> t -> unit
  (** [print_to_dot fmt z] prints a ZDD [z] in DOT format *)

val print_to_dot_file: string -> t -> unit
  (** [print_to_dot f z] outputs ZDD [z] in DOT format in file [f] *)

val stat: unit -> int
  (** Returns the total number of unique ZDD built so far *)

