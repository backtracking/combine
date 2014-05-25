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

module type Time = sig
  val gettimeofday: unit -> float
end

module type N = sig
  type t
  val zero: t
  val one: t
  val add: t -> t -> t
  val print : Format.formatter -> t -> unit
end


module Make : functor (T : Time) -> functor (N : N) -> sig

  val debug: bool ref

  type error
  val print_error : Format.formatter -> error -> unit
  exception Error of Ast.pos * error

  val interp: Format.formatter -> Format.formatter -> Ast.queue -> unit
  val interp_problems: Format.formatter -> Format.formatter ->
    Ast.queue -> Tiling.Problem.problem list

end
