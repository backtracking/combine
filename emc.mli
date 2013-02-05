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

(** {2 Exact Matrix Cover problem}

    Given a matrix of Booleans values (say 0 and 1), the exact matrix problem
    (EMC) is to find a subset of rows such that each column contains exactly
    one 1.
    This module provides a common interface to solve this problem using
    two different techniques: Dancing Links (module D, see also
    {!Dlx}) and Zero-Suppressed Binary Decision Diagrams (module Z, see also
    {!Zdd}).

    The EMC problem is slightly generalized as follows: the [primary] first
    columns must be covered exactly once, and the remaining columns (called
    secondary columns) must be covered at most one.
*)

type solution = int list
    (** A solution is a set of rows *)

(** A common interface for ZDD and DLX *)
module type S = sig

  type t

  val create: ?primary:int -> bool array array -> t
    (** Creates an EMC problem. *)

  val create_sparse: ?primary:int -> columns:int -> int list array -> t
    (** To be used instead of [create] when the matrix is large but sparse. *)

  val find_solution: t -> solution
    (** Returns the first solution that is found.
        Raises [Not_found] if the problem has no solution. *)

  val iter_solution: (solution -> unit) -> t -> unit
    (** [Emc.iter_solution f p] applies [f] in turn to each solution. *)

  val count_solutions: t -> int
    (** The total number of solutions. *)

  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
    (** Minimal arithmetic module to count solutions *)

  module Count(A: ARITH) : sig val count_solutions: t -> A.t end
    (** To count solutions using any arithmetic implementation *)

end

(** DLX-based implementation *)
module D: S

(** ZDD-based implementation *)
module Z: S with type t = Zdd.t

(** {2 Misc. functions for debugging purposes} *)

val print_boolean_matrix : Format.formatter -> bool array array -> unit
val print_boolean_array : Format.formatter -> bool array -> unit
val print_matrix_size: Format.formatter -> 'a array array -> unit

module Sat : sig

  type t

  val create: primary:int -> bool array array -> t
  val create_sparse: primary:int -> int list array -> t

  val print : Format.formatter -> t -> unit
  val print_in_file: string -> t -> unit

end






