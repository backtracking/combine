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

(* EMC Module *)

open Format

type solution = int list
(** a solution is a set of rows *)

let print_boolean_array fmt a =
  Array.iter (
    fun cell ->
      if cell then fprintf fmt "1"
      else fprintf fmt "0"
  ) a

(* display a boolean matrix *)
let print_boolean_matrix fmt m =
  Array.iter (
    fun col ->
      print_boolean_array fmt col;
      fprintf fmt "@\n"
  ) m

let print_matrix_size fmt p =
  fprintf fmt "%dx%d" (Array.length p) (Array.length p.(0))


module type S = sig
  type t
  val create: ?primary:int -> bool array array -> t
  val find_solution: t -> solution
  val iter_solution: (solution -> unit) -> t -> unit
  val count_solutions: t -> int
  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
  module Count(A: ARITH) : sig
    val count_solutions: t -> A.t
  end
end


module D = struct
  type t = Dlx.t
  let create = Dlx.create
  let find_solution p = Dlx.get_first_solution p
  let iter_solution f p = Dlx.iter_solution (
    fun e -> f (Dlx.list_of_solution e)) p
  let count_solutions p = Dlx.count_solutions p
  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
  module Count = functor (A: ARITH) ->
  struct
    let count_solutions p =
      let r = ref A.zero in
      iter_solution (fun _ -> r:= A.add !r A.one) p;
      !r
  end
end


module Z = struct

  open Zdd

  type t = Zdd.t

  let column ?primary j m =
    let n = Array.length m in
    (* we build the solution from bottom up, i.e. i = n-1,...,1,0 *)
    let rec build z zf i =
     (* z  = exactly one i such that m[i][j]=true
      zf = only i such that m[i][j]=false *)
      if i < 0 then z
      else if m.(i).(j) then build (construct i z zf) zf (i-1)
      else build (construct i z z) (construct i zf zf) (i-1)
    in
    let primary = match primary with
      | None -> true
      | Some p -> j < p
    in
    build (if primary then bottom else top) top (n-1)

  let inter_right_to_left cols =
    let width = Array.length cols in
    let z = ref cols.(0) in
    for j = 1 to width - 1 do
      let c = cols.(j) in
      printf "size = %d@." (size !z);
      z := inter c !z
    done;
    !z

  let inter_left_to_right cols =
    let width = Array.length cols in
    let z = ref cols.(width - 1) in
    for j = width - 2 downto 0 do
      let c = cols.(j) in
      printf "size = %d@." (size !z);
      z := inter c !z
    done;
    !z


  let inter_middle_balancing cols =
    let width = Array.length cols in
    let min = 0 in
    let max = width - 1 in
    let rec balancing min max =
      let mid = min + (max - min) / 2 + ((max - min) mod 2) in
      if min = max then ()
      else begin
        for j = min to mid - 1 do
          cols.(mid + (j - min)) <- inter cols.(mid + (j - min)) cols.(j)
        done;
        balancing mid max
      end
    in
    balancing min max;
    cols.(max)

  let first_row j m =
    let h = Array.length m in
    let rec lookup i = if m.(i).(j) || i = h then i else lookup (i+1) in
    lookup 0

  let tiling ?primary m =
    let width = Array.length m.(0) in
    let cols =
      Array.init width (fun j -> first_row j m, column ?primary j m) in
    let compare (i1, _) (i2, _) = Pervasives.compare i2 i1 in
    Array.sort compare cols;
    inter_middle_balancing (Array.map snd cols)


  let create ?primary m = tiling ?primary m
  let find_solution p = choose_list p
  let iter_solution f p = iter_list f p
  let count_solutions p = cardinal p


  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
  module Count(A: ARITH) = struct
    module C = Cardinal(A)
    let count_solutions p = C.cardinal p
  end
end





















