open Format

exception NoBoard


let msg = "usage: project [options] file"
let spec = [] 

let file = ref None
let set_file f = match !file with
  | Some _ -> Arg.usage spec msg; exit 1
  | None when Sys.file_exists f -> file := Some f
  | None -> eprintf "%s: no such file@." f; exit 1

let () = Arg.parse spec set_file msg

let error_pieces_board () = 
  eprintf "problem must have board and piece(s) @."; 
  exit 1 

let problem =
  let c = match !file with
    | Some f -> open_in f
    | None -> stdin
  in
  let p = 
    try
      Parser.read_problem c
    with Invalid_argument msg ->
      eprintf "invalid input file: %s@." msg;
      exit 1
  in
  begin match !file with Some _ -> close_in c | None -> () end;
  p

open Zdd

let m = Tiling.emc problem
let () = Tiling.display_boolean_matrix m
let () = printf "%d x %d@." (Array.length m) (Array.length m.(0))

let () = 
  printf "ZDD : solutions : %d@." 
    (Zdd.cardinal (Zdd.tiling m));
(*
  printf "ZDD : solutions : %d@." 
    (Zdd.cardinal (Zdd.tiling (Tiling.emc problem)));
  printf "DLX : solutions : %d@." 
    (Dlx.count_solutions (Tiling.emc problem));

  let z = Zdd.column 0 [|[|true|];
			 [|true|];
			 [|false|];
			 [|true|];
			 [|false|];|] in
  
  let c = open_out "test.dot" in
  let fmt = Format.formatter_of_out_channel c in
  Zdd.print_to_dot fmt z;
  close_out c;
  ignore (Sys.command "dot -Tps test.dot | gv -")
*)
()

  







