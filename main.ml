
(*
let l = int_of_string Sys.argv.(1)
let h = int_of_string Sys.argv.(2)


let board = Array.create_matrix l h true


let _ = 
  Tiling.display_boolean_matrix board
*)

open Format

let msg = "usage: project [options] file"
let spec = [] 

let file = ref None
let set_file f = match !file with
  | Some _ -> Arg.usage spec msg; exit 1
  | None when Sys.file_exists f -> file := Some f
  | None -> eprintf "%s: no such file@." f; exit 1

let () = Arg.parse spec set_file msg

let c = match !file with
  | Some f -> open_in f
  | None -> stdin

let pl = Parser.read_channel c

let () = match !file with Some _ -> close_in c | None -> ()

let () = printf "%d pieces@." (List.length pl)

(*
let n = Array.length p
let () =
  if n = 0 then begin eprintf "problem height must be positive@."; exit 1 end
let m = Array.length p.(0)

let () = printf "problem has size %d x %d@." n m
*)


