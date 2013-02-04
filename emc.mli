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

(** {2 Exact Matrix Cover problem interface for ZDD and DLX} *)


type solution = int list
    (** a solution is a set of rows *)

(** a common interface for ZDD and DLX *)
module type S = sig

  type t

  val create: ?primary:int -> bool array array -> t
    (** construct the algorithm corresponding structure
       doubly linked matrix for dlx and a zdd for zdd*)

  val create_sparse: ?primary:int -> columns:int -> int list array -> t

  val find_solution: t -> solution
    (** raises [Not_found] if the problem has no solution *)

  val iter_solution: (solution -> unit) -> t -> unit
    (** [Emc.iter_solution f p] applies [f] in turn to each problem [p]
      solutions *)

  val count_solutions: t -> int
    (** Return the number of solutions to this problem *)

  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
    (** simple arithmetic module with big integers *)

  module Count(A: ARITH) : sig val count_solutions: t -> A.t end
    (** Functor usage of arithmetics *)

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
  val print : Format.formatter -> t -> unit
  val print_sat : Format.formatter -> bool array array -> unit
  val print_sat_file : string -> bool array array -> unit

  val conj_of_matrix : bool array array -> t

end






