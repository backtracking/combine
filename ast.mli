
type dim = int * int

type binop = Union | Inter | Diff | Xor

type algo = Dlx | Zdd

type setop = Shift | SetXY of bool | Resize | Crop of dim
type problem_command = Count of algo | Solve of algo | Print 
type compop = Equal

type pos = Lexing.position * Lexing.position


type expr = {
  expr_node : expr_node;
  expr_pos : pos;
}
and expr_node = 
  | Constant of bool array array
  | Var of string
  (* other operations: union, diff, rotations, etc. *)
  | Binary of binop * expr * expr
  | SetOp of setop * dim * expr
  | Apply of Tiling.Iso.t * expr

type bool_expr = 
  | Boolean of bool
  | Comparison of compop * expr * expr

type tile = expr * Tiling.Tile.symetries * Tiling.Tile.multiplicity

type tiles =
  | Tiles_id of string
  | Tiles_list of tile list

type problem = string * expr * tiles

type decl = {
  decl_node : decl_node;
  decl_pos : pos;
}

and decl_node = 
  | Pattern of string * expr
  | Tiles of string * tile list
  | Problem of problem
  | Assert of bool_expr
  | Command of problem_command * string


type file = decl list
