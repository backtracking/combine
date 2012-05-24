
open Ast
open Tiling

let var_env = Hashtbl.create 50

let problem_env = Hashtbl.create 50

let rec interp_expr = function
  | Var s -> begin
      try
        let value = Hashtbl.find var_env s in
        value
      with Not_found -> failwith ("Error : Unbound value" ^ s) end
  | Pattern m -> Pattern.create m
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

let tile e = 
  let p = interp_expr e in
  let name = match e with Var id -> Some id | _ -> None in
  Tile.create ?name p

let interp_decl = function
  | Tile (id, z) ->
      let value = interp_expr z in
      Hashtbl.replace var_env id value
  | Problem (id, e, el) ->
      let value = interp_expr e in
      Hashtbl.replace problem_env id (
        value, 
        List.map (
          fun e -> 
            tile e
        ) el
      )

let interp dl =
  let problems = ref [] in
  Hashtbl.clear var_env;
  Hashtbl.clear problem_env;
  List.iter (fun d -> interp_decl d) dl;
  Hashtbl.iter (
    fun id value -> 
      let g, pl = value in 
      let problem = 
        Tiling.create_problem ?name:(Some id) g pl in
      problems := problem :: !problems
  ) problem_env;
  !problems

