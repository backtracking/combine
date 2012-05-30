
type dim = int * int 

type binop = Union | Inter | Diff | Xor

type setop = Shift | SetXY of bool | Resize | Crop of dim  


type expr =
  | Constant of bool array array
  | Var of string
  (* other operations: union, diff, rotations, etc. *)
  | Binary of binop * expr * expr
  | SetOp of setop * dim * expr

type tile = expr * Tiling.Tile.symetries * Tiling.Tile.multiplicity

type decl =
  | Pattern of string * expr
  | Problem of string * expr * tile list

type file = decl list
