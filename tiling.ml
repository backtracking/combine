(* Tiling Module *)

(* Misc *)

type iso = Id | Rot90h | Rot90a | Rot180 | VertRef | HorizRef |
              Diag13Ref | Diag24Ref

module Iso = 
  Set.Make 
    (struct
       type t = iso
       let compare iso1 iso2 = if iso1 = iso2 then 0 else 1
     end)

type piece = {
  mutable matrix : bool array array;
  mutable name : string;
  mutable quantity : int;
  mutable exact : bool; 
  mutable isos_piece : Iso.t
}

type problem = {
  mutable grid : bool array array;
  mutable pname : string;
  mutable pieces : piece list;
  mutable isos_grid: Iso.t
}

let create_problem ?(n = "") g ps = {
  grid = g; 
  pname = n; 
  pieces = ps;
  isos_grid = Iso.empty
}

let create_piece ?(q = 0) ?(e = false) ?(n = "") m ={
  matrix = m; 
  name = n; 
  quantity = q; 
  exact = e;
  isos_piece = Iso.empty
}



(* display a boolean matrix *)
let display_boolean_matrix m = 
  Array.iter (
    fun col -> Array.iter (
      fun cell -> 
        if cell then Format.printf "True "
        else Format.printf "False "
    ) col; Format.printf "@."
  ) m

(* Board position testing *)

(* return true if position x y is on the board *)
let existing_position board x y = 
  x < Array.length board 
  && y < Array.length board.(0) 
  && board.(x).(y)

(* return true if piece could be put at position x y*)
let is_possible_position piece board x y =
  try
    for i = 0 to Array.length piece.matrix - 1 do
      for j = 0 to Array.length piece.matrix.(0) - 1 do
        if piece.matrix.(i).(j) 
        && not (existing_position board (x + i) (y + j))
        then raise Exit
      done
    done;
    true
  with 
    | Exit -> false

(* Isometries *)

let comput_coordinates iso (x, y) h l =
  match iso with 
    | Id -> (x, y)
    | Rot90a -> (h - y, x)
    | Rot180 -> (l - x , h - y)
    | Rot90h -> (y, l - x)
    | HorizRef -> (x, h - y)
    | VertRef -> (l - x, y)
    | Diag13Ref -> (y, x) 
    | Diag24Ref -> (h - y, l - x)

let comput_size iso (h, l) = 
	match iso with 
		| Id  
		| HorizRef 
		| VertRef  
		| Rot180 -> (h, l)
		| Rot90a  
		| Rot90h  
		| Diag13Ref   
		| Diag24Ref -> (l, h)


let apply iso matrix = 
  let l, h = Array.length matrix, Array.length matrix in
  let l_new, h_new = comput_size iso (l, h) in
  let new_m = Array.make_matrix h_new l_new false in
    Array.iteri (
      fun i col -> Array.iteri (
        fun j cell -> 
          let new_i, new_j = comput_coordinates iso (i, j) h l in
            new_m.(new_i).(new_j) <- cell
      ) col
    ) matrix;
    new_m

let get_isos matrix = 
  let iso_set = ref Iso.empty in
  let table = Hashtbl.create 8 in
    Hashtbl.add table (matrix) Id;
    Hashtbl.add table (apply Rot90a matrix) Rot90a;
    Hashtbl.add table (apply Rot90h matrix) Rot90h;
    Hashtbl.add table (apply Rot180 matrix) Rot180;
    Hashtbl.add table (apply HorizRef matrix) HorizRef;
    Hashtbl.add table (apply VertRef matrix) VertRef;
    Hashtbl.add table (apply Diag13Ref matrix) Diag13Ref;
    Hashtbl.add table (apply Diag24Ref matrix) Diag24Ref;
    Hashtbl.iter (
      fun _ iso -> iso_set := Iso.add iso !iso_set 
    ) table;
    !iso_set


(* Symmetries *)

(* return true if a piece has a diagonal symmetry *)
let has_diagonal_symmetry piece =
  let h = Array.length piece.matrix.(0) in 
  let l = Array.length piece.matrix in 
    try
      for i = 0 to Array.length piece.matrix / 2 - 1 do
        for j = 0 to h / 2 - 1 do
          if piece.matrix.(i).(j) <> piece.matrix.(l - i).(h - j) 
          then raise Exit
        done
      done;
      true
    with 
      | Exit -> false

(* return true if a piece has an horizontal symmetry *)
let has_horizontal_symmetry piece = 
  let h = Array.length piece.matrix.(0) in 
    try
      for i = 0 to Array.length piece.matrix / 2 - 1 do
        for j = 0 to h / 2 - 1 do
          if piece.matrix.(i).(j) <> piece.matrix.(i).(h - j) 
          then raise Exit
        done
      done;
      true
    with 
      | Exit -> false

(* return true if a piece has a vertical symmetry *)
let has_vertical_symmetry piece = 
  let l = Array.length piece.matrix in 
    try
      for i = 0 to l / 2 - 1 do
        for j = 0 to Array.length piece.matrix.(0) / 2 - 1 do
          if piece.matrix.(i).(j) <> piece.matrix.(i).(j) 
          then raise Exit
        done
      done;
      true
    with 
      | Exit -> false


   (* return a list of pieces containing the rotations of the piece, 
    including itself 
    *)
(* TODO : Modify and add 180° and 90° rotations*)
(*
let get_rotations piece = 
  let rec rec_get i piece rotations = 
    if i <> 3 then
      let rot_piece = create_piece piece.matrix in 
        if List.mem rot_piece rotations then
          rec_get (i + 1) rot_piece rotations
        else 
          rec_get (i + 1) rot_piece (rot_piece::rotations);
    else rotations
  in 
    rec_get 0 piece [piece]
    

(* return a list of pieces containing rotations of all start pieces *)
let get_all_rotations pieces = 
  let rec rec_add_rotations pieces with_rotations = 
    match pieces with
      | [] -> with_rotations
      | h::t -> rec_add_rotations t (get_rotations h)@with_rotations
  in 
    rec_add_rotations pieces []

*)



(* Placing piece on board *)

(* return an array of size n representing the way to put piece p at 
 position x y on the board of size l*n
 *)
let one_line l n piece_id piece x y = 
  let line = Array.make n false in
    for i = 0 to Array.length piece.matrix - 1 do  
      for j = 0 to Array.length piece.matrix.(0) - 1 do  
        if piece.matrix.(i).(j) then
          line.((x + i) * l + y + j) <- true;
        if piece.quantity <> 0 then begin 
          line.(piece_id) <- true;
        end 
      done 
    done;
    line 

let one_line_piece_only n piece_id piece =
  let line = Array.make n false in
    line.(piece_id) <- true;
    line

let comput_matrix_size board pieces =
  let h = Array.length board in 
  let l = Array.length board.(0) in 
  let n = l * h  in
    n + List.fold_left (
      fun acc e ->
        if e.quantity <> 0 then
          acc + e.quantity
        else 
          acc
    ) 0 pieces

(* return a boolean matrix representing the set of way to put all pieces
 * on the board
 * *)
let emc problem =
  let w = Array.length problem.grid in 
  let h = Array.length problem.grid.(0) in 
  let n = comput_matrix_size problem.grid problem.pieces in
  let piece_id = ref (h * w) in
  let lines = ref [] in 
  let add_piece i j piece = 
        if is_possible_position piece problem.grid i j then
          if piece.quantity = 0 then 
            lines := one_line w n !piece_id piece i j::!lines
          else 
            for k = 0 to piece.quantity do
              if not piece.exact then begin
                lines := one_line_piece_only n !piece_id piece::!lines;
                lines := one_line w n !piece_id piece i j::!lines;
                incr piece_id
              end
            done
  in 
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      List.iter (add_piece i j) problem.pieces; 
    done 
  done;
  Array.of_list !lines

 


