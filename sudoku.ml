(* Sudoku Module *)
open Format

let size = 9

let emc_size = size * size * 4

let zdd = ref false
let dlx = ref false


let display_sudoku_line line = 
  Array.iter (fun e -> printf "%d" e) line;
  printf "@."


let display_sudoku sudoku_array = 
  Array.iter (fun e -> display_sudoku_line e) sudoku_array;
  printf "@."


let read_sudoku_line s =
  Array.init size
    (fun i -> 
       (int_of_char s.[i]) - (int_of_char '0')) 


let read s = 
  let lines = Str.split (Str.regexp "\n") s in
  let sudoku_array = Array.make_matrix size size 0 in 
  let rec read lines i = 
    match lines with
      | [] -> ()
      | h::t -> 
          if h <> "" then begin 
            sudoku_array.(i) <- read_sudoku_line h;
            read t (i + 1)
          end else
            read t i
  in 
  read lines 0;
  sudoku_array



let msg = "usage: project file"

let file = ref None
let set_file f = match !file with
  | Some _ -> Arg.usage [] msg; exit 1
  | None when Sys.file_exists f -> file := Some f
  | None -> eprintf "%s: no such file@." f; exit 1

let () = Arg.parse [] set_file msg

let error_pieces_board () = 
  eprintf "problem must have board and piece(s) @."; 
  exit 1 

let sudoku =
  let c = match !file with
    | Some f -> open_in f
    | None -> stdin
  in
  let p = 
    try
      let s = String.make (in_channel_length c) ' ' in 
      really_input c s 0 (in_channel_length c - 1);
      read s
    with Invalid_argument msg ->
      eprintf "invalid input file: %s@." msg;
      exit 1
  in
  begin match !file with Some _ -> close_in c | None -> () end;
  p



let ok_in_cell v celli cellj sudoku = 
  for iteri = celli to celli + 2 do
    for iterj = cellj to cellj + 2 do
      if sudoku.(iteri).(iterj) = v then raise Exit 
    done
  done


let ok v i j sudoku = 
  try 
    if sudoku.(i).(j) <> 0 then raise Exit;
    let celli, cellj = (i / 3) * 3, (j / 3) * 3 in
    ok_in_cell v celli cellj sudoku;
    let rec iteri_out_cell first last = 
      if first > last then ()
      else if sudoku.(first).(j) = v then raise Exit
      else iteri_out_cell (first + 1) last
    in
    let rec iterj_out_cell first last = 
      if first > last then ()
      else if sudoku.(i).(first) = v then raise Exit
      else iterj_out_cell (first + 1) last
    in
    iteri_out_cell 0 (celli - 1);
    iteri_out_cell (celli + 3) (size - 1);
    iterj_out_cell 0 (cellj - 1);
    iterj_out_cell (cellj + 3) (size - 1);
    true
  with Exit -> false



let set_val v i j line = 
  (* Column *)
  line.(9 * j + v - 1) <- true;
  (* Line *)
  line.(9 * (9 + i) + v - 1) <- true;
  (* Group *)
  line.(9 * (9 * 2 + (j / 3 + (i / 3) * 3)) +  v - 1) <- true;
  (* Cell *)
  line.(9 * (9 * 3 + i) + j) <- true



let get_line v i j = 
  let line = Array.make emc_size false in
  set_val v i j line; 
  line


let const_line sudoku = 
  let line = Array.make emc_size false in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      let v = sudoku.(i).(j) in
      if v <> 0 then begin
        set_val v i j line
      end
    done 
  done;
  line



let emc sudoku = 
  let lr = ref [const_line sudoku] in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      for v = 1 to size do
        if ok v i j sudoku then
          lr := get_line v i j :: !lr 
      done
    done
  done;
  Array.of_list !lr




let print_emc_sudoku () = 
  for i = 1 to 9 do printf "%d        " i done;
  for i = 1 to 9 do printf "%d        " i done;
  for i = 1 to 9 do printf "%d        " i done;
  printf "@.";
  printf "colonne  ";
  for i = 1 to 8 do printf "         " done;
  printf "ligne    ";
  for i = 1 to 8 do printf "         " done;
  printf "cellule  ";
  for i = 1 to 8 do printf "         " done;
  printf "@."


let decode m emc_array i = 
  if i < Array.length emc_array - 1 then begin
    let c = ref 0 in 
    let l = ref 0 in 
    let v = ref 0 in 
    for j = 0 to 161 do 
      if emc_array.(i).(j) then begin
        if j < 81 then begin
          c := j / 9;
          v := j mod 9 + 1
        end
        else
          l := (j - 81) / 9;
      end
    done;
    assert (!v <> 0);
    m.(!l).(!c) <- !v 
  end


let () = 
  let m = sudoku in 
  let emc_array = emc m in 
  display_sudoku m;
  printf "DLX : emc_size : %dx%d @." 
    (Array.length emc_array) (Array.length emc_array.(0));
  try 
    let p = Emc.D.create emc_array in
    let s = Emc.D.find_solution p in
    let n = List.length s in 
    printf "solution size : %d@." n;
    List.iter (decode m emc_array) s;
    display_sudoku m;
    printf "%d solutions@." (Emc.D.count_solutions p)
  with Not_found -> printf "No solution@."

