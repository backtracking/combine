open Format

let zdd = ref false
let dlx = ref false
let debug = ref false
let stats = ref false

let msg = "usage: project [options] file"
let spec = ["--zdd", Arg.Set zdd, "  Count solutions using Zdd";
            "--dlx", Arg.Set dlx, "  Count solutions using Dlx";
            "--debug", Arg.Set debug, "  Set the debug flag";
            "--stats", Arg.Set stats, "  Set the stats flag";
	   ]

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

open Lexing

let () = 
  try 
    Interp.interp ptree
  with 
    | Interp.Error (pos, err) -> 
        let start, stop = pos in
        printf "File \"%s\", line %d, characters %d-%d : @\n" 
          start.pos_fname start.pos_lnum 
          (start.pos_cnum - start.pos_bol) 
          (stop.pos_cnum - stop.pos_bol) ;
        printf "Error : %a@\n" Interp.print_error err;
        exit 0
        


module N = struct
  type t = Num.num
  let zero = Num.num_of_int 0
  let one = Num.num_of_int 1
  let add = Num.add_num
  let print fmt n = Format.fprintf fmt "%s" (Num.string_of_num n)
end
module ZCount = Emc.Z.Count(N)
module DCount = Emc.D.Count(N)

open Tiling

let handle_problem p =
  printf "problem %s@\n" p.pname;
  printf "  @[%a@]@." Pattern.print p.grid;
  let primary, m, decode_tbl = Tiling.emc p in
  let solution = Emc.D.find_solution (Emc.D.create ~primary m) in 
let width, height = p.grid.Pattern.width * 100, p.grid.Pattern.height * 100 in
  Tiling.print_solution_to_svg_file "test.svg" 
    solution p decode_tbl width height;
  printf "%a@." (Tiling.print_solution_ascii p decode_tbl) solution ;
  if !debug then begin
    Emc.print_boolean_matrix m;
    printf "  %d primary columns@." primary
  end;
  if !zdd then begin
    let p = Emc.Z.create ~primary m in
    printf "  ZDD solutions: %a\n@." N.print (ZCount.count_solutions p)
  end;
  if !dlx then begin
    let p = Emc.D.create ~primary m in
    printf "  DLX solutions: %a\n@." N.print (DCount.count_solutions p)
  end



let () =
  if !stats then begin Gc.print_stat stdout; Zdd.print_stat stdout end






