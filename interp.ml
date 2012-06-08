
open Ast
open Tiling

let var_env = Hashtbl.create 50
let tiles_env = Hashtbl.create 50
let problems = ref []

let rec interp_expr = function
  | Var s -> begin
      try
        let value = Hashtbl.find var_env s in
        value
      with Not_found -> failwith ("Error : Unbound value " ^ s) end
  | Constant m -> Pattern.create m
  | Binary (op, e1, e2) -> interp_binary e1 e2 op
  | SetOp (op, d, e) -> interp_setop d e op

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

  (* SetXY (e, d, b) ->
       *)

let tile ~s ~m e =
  let p = interp_expr e in
  let name = match e with Var id -> Some id | _ -> None in
  Tile.create ?name ~s ~m p

let tile_list = List.map (fun (e, s, m) -> tile ~s ~m e)

let tiles = function
  | Tiles_id id ->
      begin try Hashtbl.find tiles_env id
      with Not_found -> failwith ("Error: unbound tile list " ^ id) end
  | Tiles_list l -> tile_list l

let interp_decl = function
  | Pattern (id, z) ->
      let value = interp_expr z in
      Hashtbl.replace var_env id value
  | Tiles (id, l) ->
      Hashtbl.replace tiles_env id (tile_list l)
  | Problem (id, e, el) ->
      let value = interp_expr e in
      let p = Tiling.create_problem ?name:(Some id) value (tiles el) in
      problems := p :: !problems

let interp dl =
  problems := [];
  Hashtbl.clear var_env;
  Hashtbl.clear tiles_env;
  List.iter (fun d -> interp_decl d) dl;
  List.rev !problems

