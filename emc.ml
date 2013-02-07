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
  val create_sparse: ?primary:int -> columns:int -> int list array -> t
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
  let create_sparse = Dlx.create_sparse
  let find_solution p = Dlx.list_of_solution (Dlx.get_first_solution p)
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

  let column ?primary j n colj =
    (* we build the solution from bottom up, i.e. i = n-1,...,1,0 *)
    let rec build z zf i =
     (* z  = exactly one i such that m[i][j]=true
        zf = only i such that m[i][j]=false *)
      if i < 0 then z
      else if colj i then build (construct i z zf) zf (i-1)
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
          (* printf "%d <- %d/\\%d@." (mid + j - min) (mid + j - min) j; *)
          cols.(mid + (j - min)) <- inter cols.(mid + (j - min)) cols.(j)
        done;
        balancing mid max
      end
    in
    balancing min max;
    cols.(max)

  let first_row j h colj =
    let rec lookup i = if i = h || colj i then i else lookup (i+1) in
    lookup 0

  let create ?primary m =
    let height = Array.length m in
    let width = Array.length m.(0) in
    let make_col j =
      let colj i = m.(i).(j) in
      first_row j height colj, column ?primary j height colj in
    let cols = Array.init width make_col in
    let compare (i1, _) (i2, _) = Pervasives.compare i2 i1 in
    Array.sort compare cols;
    inter_middle_balancing (Array.map snd cols)

  let create_sparse ?primary ~columns a =
    let height = Array.length a in
    let width = columns in
    let cols = Hashtbl.create 17 in
    Array.iteri
      (fun i rowi -> List.iter (fun j -> Hashtbl.add cols j i) rowi) a;
    let col = Array.make height false in
    let make_col j =
      Array.fill col 0 height false;
      List.iter (fun i -> col.(i) <- true) (Hashtbl.find_all cols j);
      let colj i = col.(i) in
      first_row j height colj, column ?primary j height colj in
    let cols = Array.init width make_col in
    let compare (i1, _) (i2, _) = Pervasives.compare i2 i1 in
    Array.sort compare cols;
    inter_middle_balancing (Array.map snd cols)

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

  type lit = Pos of int | Neg of int

  type cnf = lit list list

  type t = {
    fmla: cnf;
    nbvars: int;
  }

  let print_lit fmt = function
    | Pos i -> fprintf fmt "%d " (i+1)
    | Neg i -> fprintf fmt "-%d " (i+1)

  let print_clause fmt l =
    List.iter (print_lit fmt) l;
    fprintf fmt "0@\n"

  let print fmt t =
    fprintf fmt "p cnf %d %d @\n" t.nbvars (List.length t.fmla);
    List.iter (print_clause fmt) t.fmla

  let print_in_file f m =
    let c = open_out f in
    let fmt = formatter_of_out_channel c in
    print fmt m;
    fprintf fmt "@.";
    close_out c

  (* Idea: one variable vi for each row (true iff the row is selected)
     and thus the formula is

       forall primary column i.
         exists row j st m[j,i]=1. vj

     and

       forall secondary column i.
       forall row j st m[j,i]=1.
       forall row j' st m[j',i]=1 and j'<> i. not(vj) or not(vj')

  *)

  let fmla_of_emc ~primary m =
    let length, width = Array.length m, Array.length m.(0) in
    let conj = ref [] in
    for i = 0 to width - 1 do
      if i < primary then begin
	let disj = ref [] in
	for j = 0 to length - 1 do
          if m.(j).(i) then disj := Pos j :: !disj
	done;
	conj := !disj :: !conj;
      end;
      for j = 0 to length -1 do if m.(j).(i) then
        for j2 = j + 1 to length -1 do if m.(j2).(i) then
          conj := [Neg j; Neg j2] :: !conj
        done
      done
    done;
    !conj

  let create ~primary m =
    { nbvars = Array.length m;
      fmla = fmla_of_emc ~primary m; }

  let create_sparse ~primary ~columns a =
    let length = Array.length a in
    let conj = ref [] in
    let a' = Array.make columns [] in
    (* change the columns list array to row list array *)
    for i = 0 to length - 1 do
      List.iter (fun e ->
	a'.(e) <- i::a'.(e)
      ) a.(i);
    done;
    let length' = Array.length a' in
    (* create all the disjunction to limit the choice of a column in the fmla *)
    let rec disj_of row = function
      | [] -> ()
      | h::t -> conj := [Neg row; Neg h] :: !conj; disj_of row t
    in
    let rec limit_col = function [] -> () | h::t -> disj_of h t; limit_col t in
    for i = 0 to length' - 1 do
      (* create the disjunction to force the choice of at least a 1 which
	 is on a primary column *)
      if primary = length' || i < primary then begin
	let disj = ref [] in
	List.iter (fun e -> disj := Pos e :: !disj) a'.(i);
	conj := !disj :: !conj;
      end;
      limit_col a'.(i);
    done;
    { nbvars = length;
      fmla = !conj; }

  type sat_solver = input:string -> output:string -> string

  let find_solution (sat_solver: sat_solver) t =
    let input = Filename.temp_file "combine" ".dimacs" in
    print_in_file input t;
    let output = Filename.temp_file "combine" ".out" in
    let sat_command = sat_solver input output in
    ignore (Sys.command sat_command);
    let c = open_in output in
    let sol = ref [] in
    try
      let s = input_line c in
      if s = "UNSAT" then raise Not_found;
      if s <> "SAT" then
        failwith (sprintf "Sat.find_solution: got '%s' from SAt solver" s);
      let s = input_line c in
      close_in c;
      let rec read s =
        let i, s = Scanf.sscanf s "%d %s@\n" (fun i s -> i, s) in
        if i = 0 then !sol
        else begin if i > 0 then sol := i-1 :: !sol; read s end
      in
      read s
    with End_of_file ->
      close_in c;
      failwith "Sat.find_solution: unexpected end of file"

end
