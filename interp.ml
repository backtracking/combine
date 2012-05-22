
open Ast
open Tiling

let rec interp_expr = function
  | Var s ->
      assert false (* TODO *)
  | Pattern m ->
      assert false (* TODO *)

let interp_decl = function
  | Tile (id, z) ->
      assert false (* TODO *)
  | Problem (id, e, el) ->
      assert false (* TODO *)

let interp dl =
  assert false (* TODO *)
