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

open Combine
open Ast
open Tiling
open Tiling.ToEMC
open Format
open Emc

type error = string
let print_error fmt error = fprintf fmt "%s" error
exception Error of pos * error

let timer = ref 0.

let debug = ref false
let timing = ref false

let var_env = Hashtbl.create 50
let tiles_env = Hashtbl.create 50
let problems = ref []
let problem_tbl = Hashtbl.create 50

let rec interp_expr expr = match expr.expr_node with
  | Var s -> begin
      try
        let value = Hashtbl.find var_env s in
        value
      with Not_found -> raise (Error
                        (expr.expr_pos, "Unbound value " ^ s)) end
  | Constant m -> Pattern.create m
  | Binary (op, e1, e2) -> interp_binary e1 e2 op
  | SetOp (op, d, e) -> interp_setop d e op
  | Apply (iso, e) -> Pattern.apply iso (interp_expr e)

and interp_binary e1 e2 = function
  | Union -> Pattern.union (interp_expr e1) (interp_expr e2)
  | Inter -> Pattern.inter (interp_expr e1) (interp_expr e2)
  | Diff -> Pattern.diff (interp_expr e1) (interp_expr e2)
  | Xor -> Pattern.xor (interp_expr e1) (interp_expr e2)

and interp_setop d e = function
  | Shift ->
      let ofsx, ofsy = d in
      Pattern.shift (interp_expr e) ~ofsx ~ofsy
  | SetXY b->
      let p = interp_expr e in
      let x, y = d in
      let m = Array.map Array.copy p.Pattern.matrix in
      m.(y).(x) <- b;
      Pattern.create m
  | Resize ->
      let w, h = d in
      Pattern.resize (interp_expr e) ~w ~h
  | Crop pos ->
      let (x, y), (w, h) = pos, d in
      Pattern.crop (interp_expr e) ~x ~y ~w ~h

let interp_bool_expr = function
  | Boolean b -> b
  | Comparison (op, e1, e2)->
      match op with
        | Equal -> (interp_expr e1) = (interp_expr e2)

let tile ~s ~m e =
  let p = (interp_expr e) in
  let name = match e.expr_node with Var id -> Some id | _ -> None in
  Tile.create ?name ~s ~m p

let tile_list = List.map (fun (e, s, m) -> tile ~s ~m e)

module N = struct
  type t = Num.num
  let zero = Num.num_of_int 0
  let one = Num.num_of_int 1
  let add = Num.add_num
  let print fmt n = Format.fprintf fmt "%s" (Num.string_of_num n)
end

module ZCount = Emc.Z.Count(N)
module DCount = Emc.D.Count(N)

let init_timer () =
  timer := Unix.gettimeofday ()

let finish_timer fmt () =
  let elapsed = Unix.gettimeofday () -. !timer in
  fprintf fmt "%fs" elapsed;
  timer := 0.

let count_emc p algo =
  let { primary = primary; matrix = m; tiles = decode_tbl } as emc =
    Tiling.ToEMC.make p in
  if !debug then printf "@[<hov 2>EMC size is %a@]@." print_emc_size emc;
  printf "%s : @?" p.pname;
  init_timer ();
  begin match algo with
    | Dlx ->
        let p = Emc.D.create ~primary m in
        printf "(DLX) %a solutions@." N.print (DCount.count_solutions p)
    | Zdd ->
        let p = Emc.Z.create ~primary m in
        if !debug then eprintf "ZDD has size %d@." (Zdd.size p);
        printf "(ZDD) %a solutions@." N.print (ZCount.count_solutions p)
    | Sat _ ->
        eprintf "cannot count solutions with a SAT solver@.";
        exit 1
  end;
  if !timing then printf "%s solutions counted in %a@." p.pname finish_timer ()

let solve_emc output p algo =
  let emc = Tiling.ToEMC.make p in
  if !debug then printf "@[<hov 2>EMC size is@\n%a@]@." print_emc_size emc;
  let { primary = primary; matrix = m; tiles = decode_tbl } = emc in
  init_timer ();
  let solution = match algo with
    | Dlx ->
        let p = Emc.D.create ~primary m in
        begin try Emc.D.find_solution p with Not_found -> [] end
    | Zdd ->
        let zdd = Emc.Z.create ~primary m in
        begin try Emc.Z.find_solution zdd with Not_found -> [] end
    | Sat sat ->
        let p = Emc.Sat.create ~primary m in
        let cmd ~input ~output =
          if !debug then eprintf "DIMACS input in %s@." input;
          sprintf "%s %s %s" sat input output
        in
        begin try Emc.Sat.find_solution cmd p with Not_found -> [] end
  in
  if solution = [] then
    printf "problem %S has no solution@\n" p.pname
  else begin
    if !timing then printf "%S solved in %a@." p.pname finish_timer ();
    match output with
      | Svg f ->
          let width, height =
            p.grid.Pattern.width * 25, p.grid.Pattern.height * 25 in
          print_solution_to_svg_file f ~width ~height p emc solution;
          printf "SVG written in file %S@." f
      | Ascii ->
          print_solution_ascii Format.std_formatter p emc solution;
          printf "@."
  end

exception Interrupt

let solve name p output =
  try
    let a = Backtracking.find name in
    init_timer ();
    let f sol =
      if !timing then printf "%S solved in %a@." p.pname finish_timer ();
      begin match output with
        | Svg f ->
            let width, height =
              p.grid.Pattern.width * 25, p.grid.Pattern.height * 25 in
            Tiling.print_solution_to_svg_file f ~width ~height p sol;
            printf "SVG written in file %S@." f
        | Ascii ->
            Tiling.print_solution_ascii Format.std_formatter p sol;
            printf "@."
      end;
      raise Interrupt
    in
    begin
      try a f p; printf "problem %S has no solution@." p.pname
      with Interrupt -> ()
    end
  with Not_found -> printf "%s: no such algorithm@." name

let count name p =
  try
    let algo = Backtracking.find name in
    init_timer ();
    let nbsol = ref 0 in
    let f _ = incr nbsol in
    algo f p;
    printf "problem %S has %d solutions@." p.pname !nbsol;
    if !timing then printf "solutions counted in %a@." finish_timer ()
  with Not_found -> printf "%s: no such algorithm@." name

let interp_problem_command p = function
  | Print -> printf "%a@\n" Tiling.print_problem p
  | SolveEMC (algo, output) -> solve_emc output p algo
  | CountEMC algo -> count_emc p algo
  | Count name -> count name p
  | Solve (name, output) -> solve name p output

let tiles = function
  | Tiles_id id ->
      begin
        try Hashtbl.find tiles_env id
        with Not_found ->
          eprintf "Error: unbound tile list %s@." id; exit 1
      end
  | Tiles_list l -> tile_list l

let rec interp_decl decl =
  match decl.decl_node with
    | Pattern (id, z) ->
        let value = interp_expr z in
        Hashtbl.replace var_env id value
    | Tiles (id, l) ->
        Hashtbl.replace tiles_env id (tile_list l)
    | Problem (id, e, el) ->
        let value = interp_expr e in
        let p = Tiling.create_problem ?name:(Some id) value (tiles el) in
        problems := p :: !problems;
        Hashtbl.add problem_tbl id p
    | Dimacs (id, file) ->
        let p = begin try Hashtbl.find problem_tbl id with
          | Not_found ->
              raise (Error (decl.decl_pos, "Unbound problem " ^ id)) end in
        let emc = Tiling.ToEMC.make p in
        let sat = Sat.create ~primary:emc.primary emc.matrix in
        Emc.Sat.print_in_file file sat
    | Assert be ->
        if not (interp_bool_expr be) then begin
          raise (Error (decl.decl_pos, "Assert failure")) end
    | Command (c, id)->
        let p = begin try Hashtbl.find problem_tbl id with
          | Not_found ->
              raise (Error (decl.decl_pos, "Unbound problem " ^ id)) end in
        interp_problem_command p c
    | Debug st ->
        debug := (match st with On -> true | Off -> false)
    | Timing st ->
        timing := (match st with On -> true | Off -> false)
    | Quit ->
        printf "exit@."; exit 0
    | Include s ->
        let file =
          if Filename.is_relative s then
            let f = (fst decl.decl_pos).Lexing.pos_fname in
            Filename.concat (Filename.dirname f) s
          else
            s
        in
        if not (Sys.file_exists file) then
          raise (Error (decl.decl_pos, "No such file: " ^ file));
        let ptree = Lexer.parse_file file in
        interp ptree
    | H2g2 -> printf "42@\n"

and interp dl =
  problems := [];
  Hashtbl.clear var_env;
  Hashtbl.clear tiles_env;
  List.iter (fun d -> interp_decl d) dl

let interp_problems dl =
  problems := [];
  Hashtbl.clear var_env;
  Hashtbl.clear tiles_env;
  List.iter (fun d -> interp_decl d) dl;
  List.rev !problems

