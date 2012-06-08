
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

type tiles =
  | Tiles_id of string
  | Tiles_list of tile list

type decl =
  | Pattern of string * expr
  | Tiles of string * tile list
  | Problem of string * expr * tiles

type file = decl list
