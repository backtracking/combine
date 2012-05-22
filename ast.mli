
type binop = Union | Inter | Diff | Xor

type expr =
  | Pattern of bool array array
  | Var of string
  (* other operations: union, diff, rotations, etc. *)
  | Binary of binop * expr * expr

type decl =
  | Tile of string * expr
  | Problem of string * expr * expr list

type file = decl list
