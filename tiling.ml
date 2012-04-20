(* Tiling Module *)

(* Misc *)
(*
type node = {
  mutable c : node;
  mutable s: int;
  mutable up: node;
  mutable down: node;
  mutable left: node;
  mutable right: node;
  mutable name : string;
}
*)


type piece = {
  mutable matrix : bool array array;
  mutable name : string;
  mutable quantity : int;
  mutable exact : bool; 
}


let create_piece ?(q = 0) ?(e = false) ?(n = "") m =
  {matrix = m; name = n; quantity = q; exact = e}

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


(* Matrix transormations *)


(* rotate a matrix 180° *)
let half_turn m = 
  let h = Array.length m.(0) in 
  let l = Array.length m in 
  let new_m = Array.make_matrix l h false in
    Array.iteri (
      fun i col -> Array.iteri (
        fun j cell -> 
          new_m.(l - 1 - i).(h - 1 - j) <- cell
      ) col
    ) m;
    new_m

(* rotate a matrix clockwise (90°)*)
let quarter_turn_clockwise m = 
  let old_l = Array.length m in 
  let new_m = Array.make_matrix (Array.length m.(0)) old_l false in
    Array.iteri (
      fun i col -> Array.iteri (
        fun j cell -> 
          new_m.(j).(old_l - 1 - i) <- cell
      ) col
    ) m;
    new_m

(* rotate a matrix anti-clockwise (90°)*)
let quarter_turn_anticlockwise m = 
  let old_h = Array.length m.(0) in 
  let new_m = Array.make_matrix old_h (Array.length m) false in
    Array.iteri (
      fun i col -> Array.iteri (
        fun j cell -> 
          new_m.(old_h - 1 - j).(i) <- cell
      ) col
    ) m;
    new_m


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
let one_line l n piece x y = 
  let line = Array.make n false in
    for i = 0 to Array.length piece.matrix - 1 do  
      for j = 0 to Array.length piece.matrix.(0) - 1 do  
        if piece.matrix.(i).(j) then
          line.((x + i) * l + y + j) <- true
      done 
    done;
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
let matrix_of_game board pieces =
  let h = Array.length board in 
  let l = Array.length board.(0) in 
  let n = comput_matrix_size board pieces in
  let lines = ref [] in 
  let add_piece piece = 
    for i = 0 to h - 1 do
      for j = 0 to l - 1 do
        if is_possible_position piece board i j then
          lines := one_line l n piece i j::!lines
      done 
    done;
  in 
    List.iter add_piece pieces; 
    Array.of_list !lines

  


