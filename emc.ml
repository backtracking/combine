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




module Sat = struct

  type t =
    | Not of t
    | Or of t * t
    | And of t * t
    | Lit of int 
    | True 
    | False


  let tand = function
    | False, _ -> False
    | _ , False -> False
    | p , True -> p
    | True , p -> p
    | p1, p2 -> And (p1, p2)


  let tor = function
    | True, _ -> True
    | _ , True -> True
    | p , False -> p
    | False , p -> p
    | p1, p2 -> Or (p1, p2)

  let rec print fmt = function 
    | Not p -> fprintf fmt "-%a" print p
    | Or (p1, p2) -> fprintf fmt "%a %a" print p1 print p2
    | And (p1, p2) -> fprintf fmt "%a 0@\n%a" print p1 print p2
    | Lit i -> fprintf fmt "%d" (i + 1)
    | True -> fprintf fmt "True"
    | False -> fprintf fmt "Talse"

  let disj_of_column m i = 
    let disj, _ = Array.fold_left (
      fun (acc, j) e -> 
        if e then (tor (acc, Lit j), j + 1)
        else (acc, j + 1)
    ) (False, 0) m.(i) in 
    disj


  let conj_of_matrix m =
    let length, width = Array.length m, Array.length m.(0) in
    let conj = ref True in 
    for i = 0 to width - 1 do
      let disj = ref False in 
      for j = 0 to length - 1 do
        if m.(j).(i) then 
          disj := tor ((!disj), (Lit j))
      done;
      conj := tand (!conj, !disj);
      for j = 0 to length -1 do
        if m.(j).(i) then begin
          for j2 = j + 1 to length -1 do
            if m.(j2).(i) then
              conj := tand (!conj, Or (Not (Lit j), Not (Lit j2)))
          done 
        end
      done
    done;
    !conj


  let rec nb_clauses cnf = 
    match cnf with 
      | And (p1, p2) -> (nb_clauses p1) + (nb_clauses p2)
      | _ -> 1

  let print_sat fmt m = 
    let p = conj_of_matrix m in 
    fprintf fmt "p cnf %d %d@\n" (Array.length m) (nb_clauses p);
    fprintf fmt "%a 0@\n" print p

  let print_sat_file f m = 
    let c = open_out f in
    let fmt = formatter_of_out_channel c in
    print_sat fmt m



(*
  let () = 

    let m = [|[|false; true; false|];
              [|true; false; true|];
              [|true; false; false|];
             [|false; true; true|]|] in 

    let p = conj_of_matrix m in

    printf "%a@." print p
*)




end















