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

let ptree = match !file with
  | Some f -> Lexer.parse_file f
  | None -> exit 0

let problems = Interp.interp ptree

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

  




