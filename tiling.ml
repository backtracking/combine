(* Tiling Module *)

(* Misc *)


module Iso = struct
  type t = Id | Rot90 | Rot180 | Rot270 | VertRefl | HorizRefl |
               Diag1Refl | Diag2Refl

  type iso = t

  module S = Set.Make (
  struct
    type t = iso
    let compare = Pervasives.compare 
  end)

  let compose i1 i2 = match i1, i2 with 
    | Id, a | a, Id -> a
    (* rotations *)
    | Rot90, Rot270
    | Rot270, Rot90
    | Rot180, Rot180 -> Id
    | Rot180, Rot270
    | Rot270, Rot180 -> Rot90
    | Rot90, Rot90
    | Rot270, Rot270 -> Rot180
    | Rot180, Rot90
    | Rot90, Rot180 -> Rot270
    (* reflections *)
    | VertRefl, VertRefl
    | HorizRefl, HorizRefl
    | Diag1Refl, Diag1Refl
    | Diag2Refl, Diag2Refl -> Id
    | VertRefl, Diag2Refl
    | Diag2Refl, HorizRefl
    | HorizRefl, Diag1Refl
    | Diag1Refl, VertRefl -> Rot90
    | VertRefl, HorizRefl
    | HorizRefl, VertRefl
    | Diag2Refl, Diag1Refl
    | Diag1Refl, Diag2Refl -> Rot180
    | HorizRefl, Diag2Refl
    | Diag2Refl, VertRefl
    | VertRefl, Diag1Refl
    | Diag1Refl, HorizRefl -> Rot270
    (* rotation/reflection *)
    | Rot90, VertRefl
    | Rot270, HorizRefl
    | Rot180, Diag2Refl
    | VertRefl, Rot270
    | HorizRefl, Rot90
    | Diag2Refl, Rot180 -> Diag1Refl
    | Rot90, HorizRefl
    | Rot180, Diag1Refl
    | Rot270, VertRefl 
    | VertRefl, Rot90 
    | HorizRefl, Rot270
    | Diag1Refl, Rot180 -> Diag2Refl
    | Rot90, Diag1Refl
    | Rot180, VertRefl
    | Rot270, Diag2Refl
    | VertRefl, Rot180 
    | Diag1Refl, Rot270
    | Diag2Refl, Rot90 -> HorizRefl
    | Rot90, Diag2Refl 
    | Rot180, HorizRefl
    | Rot270, Diag1Refl
    | HorizRefl, Rot180
    | Diag1Refl, Rot90 
    | Diag2Refl, Rot270 -> VertRefl
 

 let to_string = function
   | Id -> "Id"
   | HorizRefl -> "HorizRefl"
   | VertRefl -> "VertRefl"
   | Rot180 -> "Rot180"
   | Rot270 -> "Rot270"
   | Rot90 -> "Rot90" 
   | Diag1Refl -> "Diag1Refl"  
   | Diag2Refl -> "Diag2Refl"

  let all = 
    ( S.add Id 
    ( S.add Rot90
    ( S.add Rot270
    ( S.add Rot180
    ( S.add VertRefl
    ( S.add HorizRefl
    ( S.add Diag2Refl
    ( S.add Diag1Refl S.empty))))))))

  let positive = function 
    | Id | Rot90 | Rot180 | Rot270 -> true
    | VertRefl | HorizRefl | Diag1Refl | Diag2Refl -> false

  (* sanity checks for compose *)

  (* 1. positivity rules *)
  let () = 
    S.iter (fun a -> 
    S.iter (fun b -> assert (
      positive (compose a b) = (positive a = positive b))
    ) all ) all 

  (* 2. compose is associative *)
  let () = 
    S.iter (
      fun a -> 
        S.iter (
          fun b -> 
            S.iter (
              fun c -> 
                assert (compose a (compose b c) = compose (compose a b) c)) all
        ) all ) all 

  let apply iso =
    match iso with 
      | Id -> fun ?(w = 0) ?(h = 0) p -> p
      | Rot180 -> fun ?(w = 0) ?(h = 0) (x, y) -> (w - 1 - x , h - 1 - y)
      | HorizRefl -> fun ?(w = 0) ?(h = 0) (x, y) -> (x, h - 1 - y)
      | VertRefl -> fun ?(w = 0) ?(h = 0) (x, y) -> (w - 1- x, y)
      | Rot90 -> fun ?(w = 0) ?(h = 0) (x, y) -> (h - 1 - y, x)
      | Rot270 -> fun ?(w = 0) ?(h = 0) (x, y) -> (y, w - 1 - x)
      | Diag1Refl -> fun ?(w = 0) ?(h = 0) (x, y) -> (y, x) 
      | Diag2Refl -> fun ?(w = 0) ?(h = 0) (x, y) -> (h - 1 - y, w - 1 - x)

  let trans_size iso (h, l) = 
    match iso with 
      | Id  
      | HorizRefl 
      | VertRefl  
      | Rot180 -> (h, l)
      | Rot270  
      | Rot90  
      | Diag1Refl   
      | Diag2Refl -> (l, h)

  let print fmt iso = Format.fprintf fmt "%s" (to_string iso)

end


module Pattern = struct 

  type t = {
    grid   : bool array array;
    height : int;
    width  : int;
  }

  let create g =
    let h = Array.length g in
    if h = 0 then invalid_arg "Pattern.create";
    { grid = g;
      height = h;
      width = Array.length g.(0); }

  let apply iso p = 
    let trans = Iso.apply iso in
    let w, h = p.width, p.height in
    let new_w, new_h = Iso.trans_size iso (w, h) in
    let new_m = Array.make_matrix new_h new_w false in
    for y = 0 to h-1 do
      for x = 0 to w-1 do
        let new_x, new_y = trans ~w ~h (x, y)  in
        new_m.(new_y).(new_x) <- p.grid.(y).(x)
      done
    done;
    { grid = new_m; height = new_h; width = new_w }







(*
  let f_fig = 
    [|
     [|true; true; true; true|];
     [|true; false; false; false|];
     [|true; true; true; false|];
     [|true; false; false; false|];
     [|true; false; false; false|];
    |]

  let genere () = 
    Iso.S.iter (
      fun a -> 
        Iso.S.iter (
          fun b -> 
            Iso.S.iter (
              fun c -> 
                try 
                  let m1 = apply b (apply a f_fig) in
                  let m2 = apply c f_fig in 
                  if m1 = m2 then raise Exit 
                with 
                  | Exit -> 
                    Format.printf "| %a, %a -> %a@." 
                      Iso.print a Iso.print b Iso.print c
            ) Iso.all ) Iso.all ) Iso.all

  let () = 
    genere ()
*)

  let print fmt p = 
    for y = p.height-1 downto 0 do
      Array.iter (
        fun cell -> 
          if cell then Format.fprintf fmt "#"
          else Format.fprintf fmt "'"
      ) p.grid.(y);
      if y > 0 then Format.fprintf fmt "@\n"
    done


  let resize p ~w ~h  = 
    let min_w, min_h = min w p.width, min h p.height in
    let m = Array.make_matrix h w false in 
    for y = 0 to min_h - 1 do
      for x = 0 to min_w - 1 do
        m.(y).(x) <- p.grid.(y).(x)
      done 
    done;
    { grid = m; height = h; width = w }



  let crop p ~x ~y ~w ~h = 
    let m = Array.make_matrix h w false in 
    for y' = y to min p.height h - 1 do
      for x' = x to min p.width w - 1 do
        m.(y' - y).(x' - x) <- p.grid.(y').(x')
      done
    done;
    { grid = m; height = h; width = w }


  let shift p ~ofsx ~ofsy = 
    let h, w = p.height, p.width in
    let m = Array.make_matrix h w false in 
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let new_x, new_y = x + ofsx, y + ofsy in
        if new_x < w && new_x >= 0 && new_y < h && new_y >= 0 then 
          m.(new_y).(new_x) <- p.grid.(y).(x)
      done
    done;
    { grid = m; height = h; width = w }

  let union p1 p2 = 
    if p1.height <> p2.height || p1.width <> p2.width then
      invalid_arg "union"
    else 
      let w, h = p1.height, p1.width in
      let m = Array.make_matrix h w false in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do 
          m.(y).(x) <- p1.grid.(y).(x) || p2.grid.(y).(x)
        done 
      done;
      { grid = m; height = h; width = w }


   let inter p1 p2 = 
    if p1.height <> p2.height || p1.width <> p2.width then
      invalid_arg "inter"
    else 
      let w, h = p1.height, p1.width in
      let m = Array.make_matrix h w false in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do 
          m.(y).(x) <- p1.grid.(y).(x) && p2.grid.(y).(x)
        done 
      done;
      { grid = m; height = h; width = w }


  let diff p1 p2 = 
    if p1.height <> p2.height || p1.width <> p2.width then
      invalid_arg "diff"
    else 
      let w, h = p1.height, p1.width in
      let m = Array.make_matrix h w false in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do 
          let a, b = p1.grid.(y).(x), p2.grid.(y).(x) in 
          m.(y).(x) <- (not a && b) || (a && not b) (* xor operation *)
        done 
      done;
      { grid = m; height = h; width = w }

    (*
    print_boolean_matrix f_fig;
    Format.printf "@.";
    print_boolean_matrix (apply Iso.Diag1Refl (apply Iso.Diag2Refl f_fig));
    Format.printf "@.";
    print_boolean_matrix (apply Iso.Diag2Refl (apply Iso.Diag1Refl f_fig))
    *)

  (* TODO: improve efficiency (do not call [apply]) *)
  let has_iso iso p = p = apply iso p

  let get_isos p = 
    Iso.S.filter (fun iso -> has_iso iso p) Iso.all

end 

module Tile = struct

  type t = {
    pattern: Pattern.t;
    isos   : Iso.S.t;   (* the pattern is invariant by these isometries *)
  }

  let create p =
    { pattern = p;
      isos    = Pattern.get_isos p; }

  let apply iso t =
    if Iso.S.mem iso t.isos then t
    else create (Pattern.apply iso t.pattern)

  let print fmt t =
    Format.fprintf fmt "%a@\n" Pattern.print t.pattern;
    Format.fprintf fmt "{ ";
    Iso.S.iter (fun iso -> Format.fprintf fmt "%a, " Iso.print iso) t.isos;
    Format.fprintf fmt "}"

end


type piece = {
  mutable matrix : bool array array;
  mutable name : string;
  mutable quantity : int;
  mutable exact : bool; 
  mutable isos_piece : Iso.S.t
}

type problem = {
  mutable grid : bool array array;
  mutable pname : string;
  mutable pieces : piece list;
  mutable isos_grid: Iso.S.t
}

let create_problem ?(n = "") g ps = {
  grid = g; 
  pname = n; 
  pieces = ps;
  isos_grid = Iso.S.empty

}


let create_piece ?(q = 0) ?(e = false) ?(n = "") m ={
  matrix = m; 
  name = n; 
  quantity = q; 
  exact = e;
  isos_piece = Iso.S.empty
}

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
  let h = Array.length problem.grid in 
  let w = Array.length problem.grid.(0) in 
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

 


