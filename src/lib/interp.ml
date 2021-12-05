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

  (* module Count = functor (A: ARITH) -> *)


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

let fast_dlx = ref false

module Make = functor (T : Time) -> functor (N : N) -> struct

  open Ast
  open Tiling
  open Tiling.Problem
  open Tiling.Problem.ToEMC
  open Format
  open Emc

  type error = string
  let print_error fmt error = fprintf fmt "%s" error
  exception Error of pos * error

  let timer = ref 0.

  let debug = ref false
  let timing = ref false

  let var_env = Hashtbl.create 50
  let var3_env = Hashtbl.create 50
  let tiles_env = Hashtbl.create 50
  let problems = ref []
  let tiles3_env = Hashtbl.create 50
  let problems3 = ref []
  let problem_tbl = Hashtbl.create 50
  let problem3_tbl = Hashtbl.create 50

  let rec interp_expr expr = match expr.expr_node with
    | Var s -> begin
      try
        let value = Hashtbl.find var_env s in
        value
      with Not_found -> raise (Error
                                 (expr.expr_pos, "Unbound value " ^ s)) end
    | Pattern m -> Pattern.create m
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
    let p = interp_expr e in
    let name = match e.expr_node with Var id -> Some id | _ -> None in
    Tile.create ?name ~s ~m p

  let tile_list = List.map (fun (e, s, m) -> tile ~s ~m e)

  let interp3_expr expr = match expr.expr_node with
    | Var s -> begin
      try
        let value = Hashtbl.find var3_env s in
        value
      with Not_found -> raise (Error
                                 (expr.expr_pos, "Unbound value " ^ s)) end
    | _ ->
      raise (Error (expr.expr_pos, "3D pattern expected"))

  let tile3 ~s ~m e =
    let p = interp3_expr e in
    let name = match e.expr_node with Var id -> Some id | _ -> None in
    Tile3.create ?name ~s ~m p

  let tile3_list = List.map (fun (e, s, m) -> tile3 ~s ~m e)

  module ZCount = Emc.Z.Count(N)
  module DCount = Emc.D.Count(N)

  let init_timer () =
    timer := T.gettimeofday ()

  let finish_timer fmt () =
    let elapsed = T.gettimeofday () -. !timer in
    fprintf fmt "%fs" elapsed;
    timer := 0.

  let count_emc fmt efmt p algo =
    let { columns = columns; primary = primary; emc = m; tiles = _decode_tbl }
        as emc = Tiling.Problem.ToEMC.make p in
    if !debug then fprintf fmt "@[<hov 2>EMC size is %a@]@." print_emc_size emc;
    fprintf fmt "%s : @?" p.pname;
    init_timer ();
    begin match algo with
    | Dlx when !fast_dlx ->
      let p = Dlxa.create_sparse ~primary ~columns m in
      fprintf fmt "(DLX) %d solutions@." (Dlxa.count_solutions p)
    | Dlx ->
      let p = Emc.D.create_sparse ~primary ~columns m in
      fprintf fmt "(DLX) %a solutions@." N.print (DCount.count_solutions p)
    | Zdd ->
      let p = Emc.Z.create_sparse ~primary ~columns m in
      if !debug then fprintf efmt "ZDD has size %d@." (Zdd.size p);
      fprintf fmt "(ZDD) %a solutions@." N.print (ZCount.count_solutions p)
    | Sat _ ->
      fprintf efmt "cannot count solutions with a SAT solver@.";
      exit 1
    end;
    if !timing then fprintf fmt "%s solutions counted in %a@." p.pname finish_timer ()

  let count3_emc fmt efmt p algo =
    let module P3 = Tiling.Problem3 in
    let { P3.ToEMC.primary = primary; emc = m; tiles = _decode_tbl } as emc =
      P3.ToEMC.make p in
    if !debug then fprintf fmt "@[<hov 2>EMC size is %a@]@."
      P3.ToEMC.print_emc_size emc;
    fprintf fmt "%s : @?" p.P3.pname;
    init_timer ();
    begin match algo with
    | Dlx ->
      let p = Emc.D.create ~primary m in
      fprintf fmt "(DLX) %a solutions@." N.print (DCount.count_solutions p)
    | Zdd ->
      let p = Emc.Z.create ~primary m in
      if !debug then fprintf efmt "ZDD has size %d@." (Zdd.size p);
      fprintf fmt "(ZDD) %a solutions@." N.print (ZCount.count_solutions p)
    | Sat _ ->
      fprintf efmt "cannot count solutions with a SAT solver@.";
      exit 1
    end;
    if !timing then
      fprintf fmt "%s solutions counted in %a@." p.P3.pname finish_timer ()

  let solve_emc fmt efmt output p algo =
    let emc = Tiling.Problem.ToEMC.make p in
    let columns = emc.columns in
    if !debug then
      fprintf fmt "@[<hov 2>EMC size is@\n%a@]@." print_emc_size emc;
    let { primary = primary; emc = m; tiles = _decode_tbl; _ } = emc in
    init_timer ();
    let solution = match algo with
      | Dlx ->
        let p = Emc.D.create_sparse ~columns ~primary m in
        begin try Emc.D.find_solution p with Not_found -> [] end
      | Zdd ->
        let zdd = Emc.Z.create_sparse ~columns ~primary m in
        begin try Emc.Z.find_solution zdd with Not_found -> [] end
      | Sat sat ->
        let p = Emc.Sat.create_sparse ~columns ~primary m in
        let cmd ~input ~output =
          if !debug then fprintf efmt "DIMACS input in %s@." input;
          sprintf "%s %s %s" sat input output
        in
        begin try Emc.Sat.find_solution cmd p with Not_found -> [] end
    in
    if solution = [] then
      fprintf fmt "problem %S has no solution@\n" p.pname
    else begin
      if !timing then fprintf fmt "%S solved in %a@." p.pname finish_timer ();
      match output with
      | Svg f ->
        let width, height =
          p.grid.Pattern.width * 25, p.grid.Pattern.height * 25 in
        print_solution_to_svg_file f ~width ~height p emc solution;
        fprintf fmt "SVG written in file %S@." f
      | Ascii ->
        print_solution_ascii fmt p emc solution;
        fprintf fmt "@."
    end

  let solve3_emc fmt efmt output p algo =
    let module P3 = Tiling.Problem3.ToEMC in
    let emc = P3.make p in
    if !debug then
      fprintf fmt "@[<hov 2>EMC size is@\n%a@]@." P3.print_emc_size emc;
    let { P3.primary = primary; emc = m; tiles = _decode_tbl } = emc in
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
          if !debug then fprintf efmt "DIMACS input in %s@." input;
          sprintf "%s %s %s" sat input output
        in
        begin try Emc.Sat.find_solution cmd p with Not_found -> [] end
    in
    if solution = [] then
      fprintf fmt "problem %S has no solution@\n" p.Problem3.pname
    else begin
      if !timing then
        fprintf fmt "%S solved in %a@." p.Problem3.pname finish_timer ();
      match output with
      | Svg _f ->
        (*TODO*)
        fprintf fmt "SVG output of 3D problems not yet implemented@."
      | Ascii ->
        P3.print_solution_ascii fmt p emc solution;
        fprintf fmt "@."
    end

  let all_emc fmt p algo =
    let module P = Tiling.Problem.ToEMC in
    let emc = P.make p in
    let columns = emc.columns in
    if !debug then
      fprintf fmt "@[<hov 2>EMC size is@\n%a@]@." P.print_emc_size emc;
    let { P.primary = primary; emc = m; tiles = _decode_tbl; _ } = emc in
    init_timer ();
    let iter_solution f = match algo with
      | Dlx ->
        let p = Emc.D.create_sparse ~columns ~primary m in
        Emc.D.iter_solution f p
      | Zdd ->
        let zdd = Emc.Z.create_sparse ~columns ~primary m in
        Emc.Z.iter_solution f zdd
      | Sat _ ->
          fprintf fmt "cannot find all solutions with SAT@."
    in
    let nb = ref 0 in
    let print1 sol =
      incr nb; P.print_solution_ascii fmt p emc sol; fprintf fmt "@." in
    iter_solution print1;
    fprintf fmt "%d solutions found for problem %S@." !nb p.Problem.pname;
    if !timing then
      fprintf fmt "%S solved in %a@." p.Problem.pname finish_timer ()

  let all3_emc fmt p algo =
    let module P3 = Tiling.Problem3.ToEMC in
    let emc = P3.make p in
    if !debug then
      fprintf fmt "@[<hov 2>EMC size is@\n%a@]@." P3.print_emc_size emc;
    let { P3.primary = primary; emc = m; tiles = _decode_tbl; _ } = emc in
    init_timer ();
    let iter_solution f = match algo with
      | Dlx ->
        let p = Emc.D.create ~primary m in
        Emc.D.iter_solution f p
      | Zdd ->
        let zdd = Emc.Z.create ~primary m in
        Emc.Z.iter_solution f zdd
      | Sat _ ->
          fprintf fmt "cannot find all solutions with SAT@."
    in
    let nb = ref 0 in
    let print1 sol =
      incr nb; P3.print_solution_ascii fmt p emc sol; fprintf fmt "@." in
    iter_solution print1;
    fprintf fmt "%d solutions found for problem %S@." !nb p.Problem3.pname;
    if !timing then
      fprintf fmt "%S solved in %a@." p.Problem3.pname finish_timer ()

  exception Interrupt

  let solve fmt efmt name p output =
    try
      let a = Backtracking.find name in
      init_timer ();
      let f sol =
        if !timing then fprintf fmt "%S solved in %a@." p.pname finish_timer ();
        begin match output with
        | Svg f ->
          let width, height =
            p.grid.Pattern.width * 25, p.grid.Pattern.height * 25 in
          Tiling.Problem.print_solution_to_svg_file f ~width ~height p sol;
          fprintf fmt "SVG written in file %S@." f
        | Ascii ->
          Tiling.Problem.print_solution_ascii Format.std_formatter p sol;
          fprintf fmt "@."
        end;
        raise Interrupt
      in
      begin
        try a f p; fprintf fmt "problem %S has no solution@." p.pname
        with Interrupt -> ()
      end
    with Not_found -> fprintf efmt "%s: no such algorithm@." name

  let count fmt efmt name p =
    try
      let algo = Backtracking.find name in
      init_timer ();
      let nbsol = ref 0 in
      let f _ = incr nbsol in
      algo f p;
      fprintf fmt "problem %S has %d solutions@." p.pname !nbsol;
      if !timing then fprintf fmt "solutions counted in %a@." finish_timer ()
    with Not_found -> fprintf efmt "%s: no such algorithm@." name

  let interp_problem_command fmt efmt p = function
    | Print -> fprintf fmt "%a@\n" Tiling.Problem.print p
    | SolveEMC (algo, output) -> solve_emc fmt efmt output p algo
    | AllEMC algo -> all_emc fmt p algo
    | CountEMC algo -> count_emc fmt efmt p algo
    | Count name -> count fmt efmt name p
    | Solve (name, output) -> solve fmt efmt name p output

  let interp_problem3_command fmt efmt p = function
    | Print -> fprintf fmt "%a@\n" Tiling.Problem3.print p
    | SolveEMC (algo, output) -> solve3_emc fmt efmt output p algo
    | AllEMC algo -> all3_emc fmt p algo
    | CountEMC algo -> count3_emc fmt efmt p algo
    | Count _name -> assert false (*TODO*)
    | Solve (_name, _output) -> assert false (*TODO*)

  let tiles efmt = function
    | Tiles_id id ->
      begin
        try Hashtbl.find tiles_env id
        with Not_found ->
          fprintf efmt "Error: unbound tile list %s@." id; exit 1
      end
    | Tiles_list l -> tile_list l

  let tiles3 efmt = function
    | Tiles_id id ->
      begin
        try Hashtbl.find tiles3_env id
        with Not_found ->
          fprintf efmt "Error: unbound tile list %s@." id; exit 1
      end
    | Tiles_list l -> tile3_list l

  let warning_duplicate kind h x =
    if Hashtbl.mem h x then eprintf "warning: duplicate %s '%s'@." kind x

  let rec interp_decl fmt efmt decl =
    match decl.decl_node with
    | Dpattern (id, z) ->
      let value = interp_expr z in
      warning_duplicate "pattern" var_env id;
      Hashtbl.replace var_env id value
    | Dpattern3 (id, l) ->
      let l = List.map interp_expr l in
      warning_duplicate "3D pattern" var3_env id;
      Hashtbl.replace var3_env id l
    | Tiles (id, l) ->
      warning_duplicate "tiles" tiles_env id;
      Hashtbl.replace tiles_env id (tile_list l)
    | Tiles3 (id, l) ->
      warning_duplicate "3D tiles" tiles3_env id;
      Hashtbl.replace tiles3_env id (tile3_list l)
    | Problem (id, e, el) ->
      let value = interp_expr e in
      let p = Tiling.Problem.create ?name:(Some id) value (tiles efmt el) in
      problems := p :: !problems;
      Hashtbl.add problem_tbl id p
    | Problem3 (id, e, el) ->
      let value = interp3_expr e in
      let p = Tiling.Problem3.create ?name:(Some id) value (tiles3 efmt el) in
      problems3 := p :: !problems3;
      Hashtbl.add problem3_tbl id p
    | Dimacs (id, file) ->
      let p = begin try Hashtbl.find problem_tbl id with
        | Not_found ->
          raise (Error (decl.decl_pos, "Unbound problem " ^ id)) end in
      let emc = Tiling.Problem.ToEMC.make p in
      let columns = emc.columns in
      let sat = Sat.create_sparse ~columns ~primary:emc.primary emc.emc in
      Emc.Sat.print_in_file file sat
    | Assert be ->
      if not (interp_bool_expr be) then begin
        raise (Error (decl.decl_pos, "Assert failure")) end
    | Command (c, id)->
      let p = begin try Hashtbl.find problem_tbl id with
        | Not_found ->
          raise (Error (decl.decl_pos, "Unbound problem " ^ id)) end in
      interp_problem_command fmt efmt p c
    | Command3 (c, id)->
      let p = begin try Hashtbl.find problem3_tbl id with
        | Not_found ->
          raise (Error (decl.decl_pos, "Unbound 3D problem " ^ id)) end in
      interp_problem3_command fmt efmt p c
    | Debug st ->
      debug := (match st with On -> true | Off -> false)
    | Timing st ->
      timing := (match st with On -> true | Off -> false)
    | Quit ->
      fprintf fmt "exit@."; exit 0
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
      interp fmt efmt ptree
    | H2g2 -> fprintf fmt "42@\n"

  and interp fmt efmt dl =
    problems := [];
    Hashtbl.clear var_env;
    Hashtbl.clear tiles_env;
    List.iter (fun d -> interp_decl fmt efmt d) dl

  let interp_problems fmt efmt dl =
    problems := []; problems3 := [];
    Hashtbl.clear var_env; Hashtbl.clear var3_env;
    Hashtbl.clear tiles_env; Hashtbl.clear tiles3_env;
    List.iter (fun d -> interp_decl fmt efmt d) dl;
    List.rev !problems, List.rev !problems3

 end
