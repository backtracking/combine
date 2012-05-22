
{
  open Format
  open Lexing
  open Parser

  type grid = char * bool array array

  let bool_array_of_string m s =
    let a = Array.create m false in
    for i = 0 to min (m - 1) (String.length s - 1) do
      if s.[i] <> ' ' then a.(i) <- true
    done;
    a

}

let space = [' ' '\t' '\n']
let comment = '#' [^ '\n']* '\n'
let letter = ['a'-'z' 'A'-'Z']
let integer = ['0'-'9']+
let ident = letter (letter | '_' | ['0'-'9'])*
let options = ("exact" space* ['1'-'9']*)

rule token = parse
  | comment
      { token lexbuf }
  | space+
      { token lexbuf }
  | "tile"
      { TILE }
  | "problem"
      { PROBLEM }
  | "false"
      { FALSE }
  | "true"
      { TRUE }
  | "constant"
      { CONSTANT }
  | "pattern"
      { PATTERN }
  | ident as id
      { IDENT id }
  | (integer as w) 'x' (integer as h)
      { DIM (int_of_string w, int_of_string h) }
  | "="
      { EQUAL }
  | "["
      { LSBRA }
  | "]"
      { RSBRA }
  | ","
      { COMMA }
  | "("
      { LPAR }
  | ")"
      { RPAR }
(*
  | '{'
      { read_lines lexbuf }
*)
  | _ as c
      { eprintf "parse error: invalid character `%c'@." c; exit 1 }
  | eof
      { EOF }

(***
and read_lines = parse
  | space* '\n' | eof
      { [] }
  | comment
      { read_lines m lexbuf }
  | '*' ([^ '\n']* as s) '\n'
      { let line = bool_array_of_string m s in
	line :: read_lines m lexbuf }
  | '}'
    { ASCII m }
  | _ as c
      { eprintf "parse error: invalid character `%c'@." c; exit 1 }
***)

{

(***
  let raw_parser c =
    let lb = from_channel c in
    read lb

  let read_problem c = 
    let pl = raw_parser c in
    if List.length pl <= 1 then invalid_arg "read_problem";
    let grid = ref None in
    let pieces = ref [] in
    let add (c, g) =
      if c = '$' then
        grid := Some g
      else
      	pieces := Tiling.create_piece ~n:(String.make 1 c) g :: !pieces
    in
    List.iter add pl;
    match !grid with
      | None -> invalid_arg "read_problem"
      | Some g -> Tiling.create_problem g !pieces
***)
}
