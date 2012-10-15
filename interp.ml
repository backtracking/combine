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

open Combine
open Ast
open Tiling
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
      Pattern.shift (interp_expr e) ?ofsx ?ofsy
  | SetXY b->
      let p = interp_expr e in
      let x, y = d in
      p.Pattern.matrix.(y).(x) <- b; p
  | Resize ->
    let w, h = d in
    Pattern.resize (interp_expr e) ?w ?h
  | Crop pos ->
    let (x, y), (w, h) = pos, d in
    Pattern.crop (interp_expr e) ?x ?y ?w ?h

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

let count p algo =
  let { primary = primary; matrix = m; tiles = decode_tbl } = Tiling.emc p in
  printf "%s : @?" p.pname;
  init_timer ();
  begin match algo with
    | Dlx ->
        let p = Emc.D.create ~primary m in
        printf "(DLX) %a solutions@." N.print (DCount.count_solutions p)
    | Zdd ->
        let p = Emc.Z.create ~primary m in
        printf "(ZDD) %a solutions@." N.print (ZCount.count_solutions p)
  end;
  if !timing then printf "%s solutions counted in %a@." p.pname finish_timer ()

let solve output p algo =
  let emc = Tiling.emc p in
  if !debug then eprintf "@[<hov 2>EMC is@\n%a@]@." print_emc emc;
  let { primary = primary; matrix = m; tiles = decode_tbl } = emc in
  init_timer ();
  let width, height =
    p.grid.Pattern.width * 25, p.grid.Pattern.height * 25 in
  let solution = match algo with
    | Dlx -> begin
        try Emc.D.find_solution (Emc.D.create ~primary m) with
          | Not_found -> [] end
    | Zdd ->
        let zdd = Emc.Z.create ~primary m in
        try Emc.Z.find_solution zdd with
          | Not_found -> []
  in
  if solution = [] then
    printf "problem %S has no solution@\n" p.pname
  else begin
    let print = begin match output with
      | Svg f ->
          printf "SVG written in file %S@\n" f;
          print_solution_to_svg_file f ~width ~height p emc;
      | Ascii ->
          print_solution_ascii Format.std_formatter p emc end in
    if !timing then printf "%S solved in %a@." p.pname finish_timer ();
    print solution
  end

let interp_problem_command p = function
  | Print -> printf "%a@\n" Tiling.print_problem p
  | Sat f ->
        printf "sat out : %s@\n" f;
        Sat.print_sat_file f ((Tiling.emc p).matrix);
  | Solve (algo, output) -> solve output p algo
  | Count algo -> count p algo


let tiles = function
  | Tiles_id id ->
      begin try Hashtbl.find tiles_env id
      with Not_found -> failwith ("Error: unbound tile list " ^ id) end
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
    | Exit ->
        printf "exit@\n"; exit 0
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

