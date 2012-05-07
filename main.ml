open Format

let zdd = ref false
let dlx = ref false

let msg = "usage: project [options] file"
let spec = ["--zdd", Arg.Set zdd, "  Count solutions using Zdd";  
            "--dlx", Arg.Set dlx, "  Count solutions using Dlx"]  

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

let m = Tiling.emc problem

(*
let ()  =
  Tiling.display_boolean_matrix m;
 *)

open Num

module A = struct
  type t = Num.num
  let zero = Num.num_of_int 0
  let one = Num.num_of_int 1
  let add = Num.add_num
end

module CZDD = Emc.Z.Count(A)

let () = 
  if !zdd then 
    printf "ZDD solutions : %s@." (string_of_num (CZDD.count_solutions m));
  if !dlx then 
    printf "DLX : solutions : %d@." (Emc.D.simple_count_solutions m)


(*
let () = 
  let c = open_out "test_on_file.txt" in
  let fmt = Format.formatter_of_out_channel c in
  let print_on_file l =
    List.iter (fun e -> Format.fprintf fmt "%d " e) l;
    Format.fprintf fmt "@."
  in 
    Emc.Z.iter_solution print_on_file m
 *)

(*
  printf "DLX : solutions : %d@." 
    (Dlx.count_solutions (Tiling.emc problem));
 *)

(*
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
()
 *)

  







