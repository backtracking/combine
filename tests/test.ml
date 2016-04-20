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

(* Test Module *)

open Combine
open Dlx
open Zdd
open Tiling
open Tiling.Problem
open Tiling.Problem.ToEMC
open Format
open Emc

module N = struct
  type t = Num.num
  let zero = Num.num_of_int 0
  let one = Num.num_of_int 1
  let add = Num.add_num
  let print fmt n = Format.fprintf fmt "%s" (Num.string_of_num n)
end
module T = struct
  let gettimeofday = Unix.gettimeofday
end

module TestInterp = Interp.Make(T)(N)

let r = Lexer.parse_file "examples/non-regression.cmb"
let problems, _ = TestInterp.interp_problems std_formatter err_formatter r
let p = List.hd problems

(* sets of sets of integers *)
module SS = struct
  include Set.Make(Zdd.S)
  let of_zdd z =
    List.fold_left (fun ss s -> add s ss) empty (Zdd.elements z)
  let to_zdd ss =
    fold Zdd.add ss Zdd.bottom
  (* n sets of <=k elements each, values in 0..m-1 *)
  let random n k m =
    let random_set () =
      let s = ref Zdd.S.empty in
      for i = 1 to Random.int (k+1) do s := Zdd.S.add (Random.int m) !s done;
      !s
    in
    let ss = ref empty in
    for i = 1 to n do ss := add (random_set ()) !ss done; !ss
  let print_set fmt s =
    fprintf fmt "{";
    Zdd.S.iter (fun x -> fprintf fmt "%d,@ " x) s; fprintf fmt "}"
  let print fmt ss =
    fprintf fmt "{"; iter (fun s -> fprintf fmt "%a,@ " print_set s) ss;
    fprintf fmt "}"
end

(* union/inter/diff *)
let () =
  for n = 0 to 10 do for k = 0 to 10 do
    let ss1 = SS.random n k 20 in
    let z1 = SS.to_zdd ss1 in
    let ss2 = SS.random n k 20 in
    let z2 = SS.to_zdd ss2 in
    assert (SS.equal (SS.union ss1 ss2) (SS.of_zdd (Zdd.union z1 z2)));
    assert (SS.equal (SS.inter ss1 ss2) (SS.of_zdd (Zdd.inter z1 z2)));
    assert (SS.equal (SS.diff  ss1 ss2) (SS.of_zdd (Zdd.diff  z1 z2)));
    assert (SS.subset  ss1 ss2 = Zdd.subset  z1 z2);
    (* || (eprintf "s1 = %a / s2 = %a@." SS.print ss1 SS.print ss2; false)); *)
  done done

let to_set l =
  let s = ref Zdd.S.empty in
  List.iter (fun e -> s := Zdd.S.add e !s) l;
  !s

let () =
  let z1 = construct 1 bottom top in
  let z2 = construct 2 bottom top in
  assert (mem (to_set [1]) z1);
  assert (mem (to_set [2]) z2);
  assert (mem (to_set [1]) (union z1 z2));
  assert (mem (to_set [2]) (union z1 z2))

let () =
  let s1 = to_set [1; 2; 3] in
  let s2 = to_set [1; 2] in
  let z1 = Zdd.singleton s1 in
  let z2 = Zdd.singleton s2 in
  assert (Zdd.mem s1 z1);
  assert (Zdd.is_empty (Zdd.remove s1 z1));
  let z3 = Zdd.union z1 z2 in
  assert (Zdd.mem s1 z3);
  assert (Zdd.mem s2 z3)

let () =
  let { columns = columns; primary = primary; emc = m; tiles = uncode_tbl } =
    Tiling.Problem.ToEMC.make p in
  assert (Emc.Z.count_solutions (Emc.Z.create_sparse ~columns ~primary m)
          = 12988816)

let p =
  Pattern.create
    [|
     [|true; true;|];
     [|true; false;|];
    |]

let p2 =
  Pattern.create
    [|
     [|true; true;|];
     [|true; true;|];
    |]


let () =
  let m = [|[|false; true; false|];
            [|true; false; true|];
            [|true; false; false|];
            [|false; true; true|]|] in
  let _sat = Sat.create ~primary:3 m in
  () (* printf "%a@." Sat.print sat *)



(*

let () = printf "%a\n@." Pattern.print (Pattern.shift p 1 1)

let () = printf "%a\n@." Pattern.print (Pattern.union p (Pattern.shift p 1 1))

let () = printf "%a\n@." Pattern.print (Pattern.inter p (Pattern.shift p 1 1))

let () = printf "%a\n@." Pattern.print (Pattern.diff p (Pattern.shift p 1 1))
let () = printf "%a\n@." Pattern.print (Pattern.diff p p)


 *)
