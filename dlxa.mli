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

(** Knuth's dancing links (aka DLX)
    see http://en.wikipedia.org/wiki/Dancing_Links

    Alternative implementation using arrays and integer indices
    instead of records and pointers.

    Experimentally, it is almost twice faster than Dlx (1.8 to be precise).
    The reason is that mutations of record fields incurs some GC bookkeeping,
    while mutation of mere integer arrays does not.
*)

type t
  (** The internal data structure for DLX *)

val create: ?primary:int -> bool array array -> t
  (** Creates a DLX data structure from a matrix of Boolean values.
      The purpose of DLX is to solve the Exact Matrix Cover problem
      for this matrix, that is to find one (or all) subsets of rows
      such that each column contains exactly one value [true] for primary
      columns and at most one [true] value for secondary columns.

      If [primary] is given, it means that the first [primary] columns are
      primary; otherwise, all columns are primary columns. *)

val count_solutions: t -> int
  (** Returns the number of solutions. *)

