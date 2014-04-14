(**************************************************************************)
(*                                                                        *)
(*  Combine - an OCaml library for combinatorics                          *)
(*                                                                        *)
(*  Copyright (C) 2012-2014                                               *)
(*    Remy El Sibaie                                                      *)
(*    Jean-Christophe Filliatre                                           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

{
  open Format
  open Lexing
  open Parser

  type grid = char * bool array array

  let lines = ref []

  let line_buffer = Buffer.create 1024
  let string_buffer = Buffer.create 1024

  let bool_array_of_string s =
    Array.init (String.length s) (fun i -> s.[i] = '*')

  let push_line () =
    let s = Buffer.contents line_buffer in
    Buffer.clear line_buffer;
    if s <> "" then lines := bool_array_of_string s :: !lines

  let create_bool_array () =
    let w = List.fold_left (fun w a -> max w (Array.length a)) 0 !lines in
    let m = Array.of_list !lines in
    lines := [];
    let adapt a =
      let n = Array.length a in
      if n = w then a else Array.init w
        (fun i -> if i < n then a.(i) else false)
    in
    Array.map adapt m

  exception Lexical_error of string

  let ident_of_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
[ "pattern",    PATTERN;
  "tiles",      TILES;
  "problem",    PROBLEM;
  "false",      FALSE;
  "true",       TRUE;
  "constant",   CONSTANT;
  "assert",     ASSERT;
  "print",      PRINT;
  "svg",        SVG_OUT;
  "ascii",      ASCII_OUT;
  "solve",      SOLVE;
  "count",      COUNT;
  "dimacs",     DIMACS;
  "include",    INCLUDE;
  "exit",       EXIT;
  "sat",        SAT;
  "dlx",        DLX;
  "answer_to_the_question_of_life_the_universe_and_everything", H2G2;
  "zdd",        ZDD;
  "debug",      DEBUG;
  "timing",     TIMING;
  "on",         ON;
  "off",        OFF;
  "diff",       DIFF;
  "union",      UNION;
  "inter",      INTER;
  "xor",        XOR;
  "set",        SET;
  "shift",      SHIFT;
  "resize",     RESIZE;
  "crop",       CROP;
  "apply",      APPLY;
  "Id",         ID;
  "Rot90",      ROT90;
  "Rot180",     ROT180;
  "Rot270",     ROT270;
  "VertRefl",   VERTREFL;
  "HorizRefl",  HORIZREFL;
  "Diag1Refl",  DIAG1REFL;
  "Diag2Refl",  DIAG2REFL;
      ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s

}

let space = [' ' '\t']
let newline = '\n'
let comment = '#' [^ '\n']* '\n'
let letter = ['a'-'z' 'A'-'Z']
let integer = ['0'-'9']+
let ident = letter (letter | '_' | ['0'-'9'])*
let options = ("exact" space* ['1'-'9']*)




rule token = parse
  | comment
      { new_line lexbuf; token lexbuf }
  | space+
      { token lexbuf }
  | newline
      { new_line lexbuf; token lexbuf }
  | "-"
      { MINUS }
  | "&&"
      { AMPAMP }
  | "||"
      { BARBAR }
  | "^"
      { HAT }
  | ident as id
      { ident_of_keyword id }
  | "\""
      { let s = string lexbuf in STRING s}
  | "~one"
      { ONE }
  | "~maybe"
      { MAYBE }
  | "~sym"
      { SYM }
  | "~rot"
      { ROT }
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
  | '{' space*
      { read_lines lexbuf }
  | '{' space* newline
      { new_line lexbuf; read_lines lexbuf }
  | _ as c
      { raise (Lexical_error (sprintf "invalid character `%c'@." c)) }
  | eof
      { EOF }

and read_lines = parse
  | space+
      { read_lines lexbuf }
  | comment | newline
      { new_line lexbuf; push_line (); read_lines lexbuf }
  | '$'
      { push_line (); read_lines lexbuf }
  | '}'
    { push_line (); ASCII (create_bool_array ()) }
  | _ as c
      { Buffer.add_char line_buffer c; read_lines lexbuf }
  | eof
      { raise (Lexical_error "unterminated pattern") }

and string = parse
  | "\""
      { let s = Buffer.contents string_buffer in
        Buffer.clear string_buffer;
        s }
  | newline
      { new_line lexbuf;
        Buffer.add_char string_buffer '\n';
        string lexbuf }
  | "\\" "\""
      { Buffer.add_char string_buffer '"';
        string lexbuf }
  | "\\" "\\"
      { Buffer.add_char string_buffer '\\';
        string lexbuf }
  | _ as c
      { Buffer.add_char string_buffer c;
        string lexbuf }
  | eof
      { raise (Lexical_error "unterminated string") }


{

  let print_loc fmt lb =
    let pos = lexeme_start_p lb in
    let c = (pos.pos_cnum - pos.pos_bol) in
    fprintf fmt "File \"%s\", line %d, characters %d-%d:" pos.pos_fname
      pos.pos_lnum c c

  let parse_file fname =
    let c = open_in fname in
    let lb = from_channel c in
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = fname };
    let p =
      try
	Parser.queue token lb
      with
	| Lexical_error msg ->
  	  eprintf "%a@\nlexical error: %s@." print_loc lb msg;
  	  exit 1
	| Parser.Error ->
	  eprintf "%a@\nsyntax error@." print_loc lb;
	  exit 1
    in
    close_in c;
    p

  let parse_string efmt s =
    let lb = from_string s in
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = "current channel" };
    let p =
      try
	Parser.queue token lb
      with
      | Lexical_error msg ->
  	fprintf efmt "%a@\nlexical error: %s@." print_loc lb msg;
  	exit 1
      | Parser.Error ->
	fprintf efmt "%a@\nsyntax error@." print_loc lb;
	exit 1
    in
    p


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
