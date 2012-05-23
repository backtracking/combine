
type dim = int * int 

type binop = Union | Inter | Diff | Xor

type setop = Shift | SetXY of bool | Resize | Crop of dim  


type expr =
  | Pattern of bool array array
  | Var of string
  (* other operations: union, diff, rotations, etc. *)
  | Binary of binop * expr * expr
  | SetOp of setop * dim * expr

type decl =
  | Tile of string * expr
  | Problem of string * expr * expr list

type file = decl list
