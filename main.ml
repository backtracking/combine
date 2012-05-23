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

let problems =
  let c = match !file with
    | Some f -> open_in f
    | None -> stdin
  in
  let lb = Lexing.from_channel c in
  let p = 
    try
      Parser.file Lexer.token lb
    with Invalid_argument msg ->
      eprintf "invalid input file: %s@." msg;
      exit 1
  in
  begin match !file with Some _ -> close_in c | None -> () end;
  Interp.interp p


open Num

module CZDD = 
  Emc.Z.Count
    ( struct
        type t = Num.num
        let zero = Num.num_of_int 0
        let one = Num.num_of_int 1
        let add = Num.add_num
      end
    )

(* imprimer tous les problemes *)
let () = 
  printf "########## Problems #########\n@." ;
  List.iter (
    fun p -> 
      printf "%a" Tiling.print_problem p;
      printf "Solutions : %s\n@." 
        (Num.string_of_num 
           (CZDD.count_solutions 
              (Emc.Z.create (Tiling.emc p))))
  ) problems

  




