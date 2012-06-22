open Ast
open Tiling
open Format
open Emc

type error = string
let print_error fmt error = fprintf fmt "%s" error
exception Error of pos * error

let var_env = Hashtbl.create 50
let tiles_env = Hashtbl.create 50
let problems = ref []
let problem_tbl = Hashtbl.create 50

let rec interp_expr expr = 
  match expr.expr_node with
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

let count p algo = 
  let primary, m, decode_tbl = Tiling.emc p in
  match algo with
  | Dlx -> 
    let p = Emc.D.create ~primary m in
    printf "  DLX solutions: %a\n@." N.print (DCount.count_solutions p)
  | Zdd -> 
    let p = Emc.Z.create ~primary m in
    printf "  ZDD solutions: %a\n@." N.print (ZCount.count_solutions p)

let solve algo p = failwith "Not implemented yet."


let interp_problem_command p = function 
  | Print -> printf "%a@\n" Tiling.print_problem p
  | Solve algo -> solve p algo 
  | Count algo -> count p algo 


let tiles = function
  | Tiles_id id ->
      begin try Hashtbl.find tiles_env id
      with Not_found -> failwith ("Error: unbound tile list " ^ id) end
  | Tiles_list l -> tile_list l

let interp_decl decl = 
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
          | Not_found -> raise 
              (Error (decl.decl_pos, "Unbound problem " ^ id)) end in
        interp_problem_command p c


let interp dl =
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

