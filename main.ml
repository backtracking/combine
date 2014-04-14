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

open Format
open Combine


let zdd = ref false
let dlx = ref false
let debug = ref false
let stats = ref false
let parse_only = ref false

let msg = "usage: project [options] file"
let spec = [ "--debug", Arg.Set debug, "  Set the debug flag";
            "--stats", Arg.Set stats, "  Set the stats flag";
	    "--parse-only", Arg.Set parse_only, "  Stop after parsing";]

let file = ref None
let set_file f = match !file with
  | Some _ -> Arg.usage spec msg; exit 1
  | None when Sys.file_exists f -> file := Some f
  | None -> eprintf "%s: no such file@." f; exit 1


module N = struct
  type t = Num.num
  let zero = Num.num_of_int 0
  let one = Num.num_of_int 1
  let add = Num.add_num
  let print fmt n = Format.fprintf fmt "%s" (Num.string_of_num n)
end
module T = struct
  let gettimeofday = Unix.gettimeofday
end

module MainInterp = Interp.Make(T)(N)


let () = Arg.parse spec set_file msg
let () = MainInterp.debug := !debug; Backtracking.debug := !debug

let error_pieces_board () =
  eprintf "problem must have board and piece(s) @.";
  exit 1

let ptree = match !file with
  | Some f -> Lexer.parse_file f
  | None -> exit 0

let () = if !parse_only then exit 0

open Lexing

let () =
  try
    MainInterp.interp std_formatter err_formatter ptree
  with
    | MainInterp.Error (pos, err) ->
        let start, stop = pos in
        printf "File \"%s\", line %d, characters %d-%d : @\n"
          start.pos_fname start.pos_lnum
          (start.pos_cnum - start.pos_bol)
          (stop.pos_cnum - stop.pos_bol) ;
        printf "Error: %a@\n" MainInterp.print_error err;
        exit 1
    | e ->
        Printexc.print_backtrace stderr; flush stderr;
        Format.eprintf "Uncaught exception:@.";
        exit 1

let () =
  if !stats then begin
    Gc.print_stat stdout;
    printf "ZDD: %d unique trees built@." (Zdd.stat ())
  end
