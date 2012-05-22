
open Ast
open Tiling

let var_env = Hashtbl.create 50

let problem_env = Hashtbl.create 50

let rec interp_expr = function
  | Var s -> begin
      try
        let value = Hashtbl.find var_env s in
        (s, value)
      with Not_found -> failwith ("Error : Unbound value" ^ s) end
  | Pattern m -> ("", m)
  | Binary (op, e1, e2) ->
      assert false (* TODO *)

let interp_decl = function
  | Tile (id, z) ->
      let _, value = interp_expr z in
      Hashtbl.replace var_env id value
  | Problem (id, e, el) ->
      let _, value = interp_expr e in
      Hashtbl.replace problem_env id (
        value, 
        List.map (
          fun e -> 
            let id, value = interp_expr e in 
            Tiling.create_piece ~n:id value
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
      let problem = Tiling.create_problem ~n:id g pl in
      problems := problem :: !problems
  ) problem_env;
  !problems

