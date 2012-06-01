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
      | Id -> fun ~w ~h p -> p
      | Rot180 -> fun ~w ~h (x, y) -> (w - 1 - x , h - 1 - y)
      | HorizRefl -> fun ~w ~h (x, y) -> (x, h - 1 - y)
      | VertRefl -> fun ~w ~h (x, y) -> (w - 1- x, y)
      | Rot90 -> fun ~w ~h (x, y) -> (h - 1 - y, x)
      | Rot270 -> fun ~w ~h (x, y) -> (y, w - 1 - x)
      | Diag1Refl -> fun ~w ~h (x, y) -> (y, x) 
      | Diag2Refl -> fun ~w ~h (x, y) -> (h - 1 - y, w - 1 - x)

  let trans_size iso (h, w) = 
    match iso with 
      | Id  
      | HorizRefl 
      | VertRefl  
      | Rot180 -> (h, w)
      | Rot270  
      | Rot90  
      | Diag1Refl   
      | Diag2Refl -> (w, h)

  let print fmt iso = Format.fprintf fmt "%s" (to_string iso)

end


module Pattern = struct 

  type t = {
    matrix   : bool array array;
    height : int;
    width  : int;
  }

  let create g =
    let h = Array.length g in
    if h = 0 then invalid_arg "Pattern.create";
    { matrix = g;
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
        new_m.(new_y).(new_x) <- p.matrix.(y).(x)
      done
    done;
    { matrix = new_m; height = new_h; width = new_w }





(* Generating compositions pattern matching source code *)

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
    Iso.S.iter ( fun a -> 
      Iso.S.iter ( fun b -> 
        Iso.S.iter ( fun c -> 
          try 
            let m1 = apply b (apply a f_fig) in
            let m2 = apply c f_fig in 
            if m1 = m2 then raise Exit 
           with 
            | Exit -> 
              Format.printf "| %a, %a -> %a@." 
              Iso.print a Iso.print b Iso.print c
        ) Iso.all 
      ) Iso.all 
    ) Iso.all

  let () = 
    genere ()
    *)

  let print fmt p = 
    for y = p.height-1 downto 0 do
      Array.iter (
        fun cell -> 
          if cell then Format.fprintf fmt "*"
          else Format.fprintf fmt "."
      ) p.matrix.(y);
      if y > 0 then Format.fprintf fmt "@\n"
    done

  let resize p ~w ~h  = 
    let min_w, min_h = min w p.width, min h p.height in
    let m = Array.make_matrix h w false in 
    for y = 0 to min_h - 1 do
      for x = 0 to min_w - 1 do
        m.(y).(x) <- p.matrix.(y).(x)
      done 
    done;
    { matrix = m; height = h; width = w }

  let crop p ~x ~y ~w ~h = 
    let m = Array.make_matrix h w false in 
    for y' = y to min p.height h - 1 do
      for x' = x to min p.width w - 1 do
        m.(y' - y).(x' - x) <- p.matrix.(y').(x')
      done
    done;
    {matrix = m; height = h; width = w }

  let shift p ~ofsx ~ofsy = 
    let h, w = p.height, p.width in
    let m = Array.make_matrix h w false in 
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let new_x, new_y = x + ofsx, y + ofsy in
        if new_x < w && new_x >= 0 && new_y < h && new_y >= 0 then 
          m.(new_y).(new_x) <- p.matrix.(y).(x)
      done
    done;
    { matrix = m; height = h; width = w }

  let binary_op op p1 p2 = 
    if p1.height <> p2.height || p1.width <> p2.width then
      invalid_arg "union";
    let w, h = p1.height, p1.width in
    let m = Array.make_matrix h w false in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do 
        m.(y).(x) <- op p1.matrix.(y).(x) p2.matrix.(y).(x)
      done 
    done;
    { matrix = m; height = h; width = w }

  let union = binary_op (||)
  let inter = binary_op (&&)
  let diff  = binary_op (fun a b -> a && not b)
  let xor = binary_op (fun a b -> a && not b || b && not a)
        
    (*
    print_boolean_matrix f_fig;
    Format.printf "@.";
    print_boolean_matrix (apply Iso.Diag1Refl (apply Iso.Diag2Refl f_fig));
    Format.printf "@.";
    print_boolean_matrix (apply Iso.Diag2Refl (apply Iso.Diag1Refl f_fig))
    *)

  let has_iso iso p =
    let w = p.width and h = p.height in
    let new_h, new_w = Iso.trans_size iso (h, w) in
    new_w = p.width && new_h = p.height &&
    let trans = Iso.apply iso in 
    try for y = 0 to p.height - 1 do
      for x = 0 to p.width -1 do
        let new_x, new_y = trans ~w ~h (x, y) in
        if p.matrix.(new_y).(new_x) <> p.matrix.(y).(x) then raise Exit
      done
    done;true with Exit -> false

  let get_isos p = 
    Iso.S.filter (fun iso -> has_iso iso p) Iso.all

end 

module Tile = struct

  type symetries = Snone | Srotations | Sall
  type multiplicity = Minf | Mone | Mmaybe

  type t = {
    name: string option;
    pattern: Pattern.t;
    multiplicity : multiplicity;
    symetries : symetries;
    isos   : Iso.S.t;   (* the pattern is invariant by these isometries *)
  }

  let create ?name ?(s=Snone) ?(m=Minf) p =
    { name = name; pattern = p;
      multiplicity = m;
      symetries = s;
      isos    = Pattern.get_isos p; }

  let apply iso t =
    if Iso.S.mem iso t.isos then t
    else create ~m:t.multiplicity (Pattern.apply iso t.pattern)


  let symetries t = 
    Iso.S.fold
      (fun iso l -> apply iso t :: l ) (Iso.S.diff Iso.all t.isos) [t] 

  let create_all_symetries t = 
    let h = Hashtbl.create 8 in
    let l = match t.symetries with
      | Snone -> Iso.S.add Iso.Id Iso.S.empty 
      | Srotations -> Iso.S.filter Iso.positive Iso.all
      | Sall-> Iso.all 
    in
    Iso.S.iter (fun iso -> Hashtbl.replace h (apply iso t) ()) 
      (Iso.S.diff l t.isos);
    Hashtbl.fold (fun k _ acc -> k :: acc) h []


  (*   let generate_all tiles =  *)





  let print fmt t =
    begin match t.name with Some s ->
    Format.printf "name : %s@\n"s; | None -> () end;
    Format.fprintf fmt "%a@\n" Pattern.print t.pattern;
    Format.fprintf fmt "{ ";
    Iso.S.iter (fun iso -> Format.fprintf fmt "%a, " Iso.print iso) t.isos;
    Format.fprintf fmt "}"

end



type problem = {
  grid : Pattern.t;
  pname : string option;
  pieces : Tile.t list;
}

let create_problem ?name g ps = {
  grid = g; 
  pname = name; 
  pieces = ps;
}




let print_problem fmt problem = 
  begin match problem.pname with 
    | Some s -> Format.fprintf fmt "name : %s@\n" s;
    | None -> () end;
  Format.fprintf fmt "%a@\n" Pattern.print problem.grid;
  List.iter (fun t -> Format.fprintf fmt "%a@\n" Tile.print t) problem.pieces

  



(* Board position testing *)

(* return true if position x y is on the board *)
let existing_position problem x y = 
  x < problem.grid.Pattern.width
  && y < problem.grid.Pattern.height
  && problem.grid.Pattern.matrix.(y).(x)

(* return true if piece could be put at position x y*)
let is_possible_position tile board x y =
  try
    for y' = 0 to tile.Tile.pattern.Pattern.height - 1 do
      for x' = 0 to tile.Tile.pattern.Pattern.width - 1 do
        if tile.Tile.pattern.Pattern.matrix.(y').(x') 
        && not (existing_position board (x + x') (y + y'))
        then raise Exit
      done
    done;
    true
  with 
    | Exit ->  false

(* Placing piece on board *)

(* return an array of size n representing the way to put piece p at 
 position x y on the board of size l*n
 *)

open Tile


let get_id_col_emc problem x y = 
  let id = ref 0 in
  try
    for y' = 0 to problem.grid.Pattern.height -1 do
      for x' = 0 to problem.grid.Pattern.width - 1 do
        if y' = y && x' = x then raise Exit;
        if problem.grid.Pattern.matrix.(y').(x') then
          incr id
      done 
    done;
    !id
  with Exit -> !id


let one_line l n tile_id tile problem x y = 
  let line = Array.make n false in
  for y' = 0 to tile.Tile.pattern.Pattern.height - 1 do  
    for x' = 0 to tile.Tile.pattern.Pattern.width - 1 do  
      if tile.Tile.pattern.Pattern.matrix.(y').(x') then begin
        line.(get_id_col_emc problem (x + x') (y + y')) <- true;
        if tile.Tile.multiplicity <> Minf then 
          line.(tile_id) <- true 
      end
    done 
  done;
  line 

let one_line_piece_only n piece_id piece =
  let line = Array.make n false in
    line.(piece_id) <- true;
    line


let number_of_cell_columns problem = 
  let h = problem.grid.Pattern.height in 
  let w = problem.grid.Pattern.width in 
  let realn = ref 0 in 
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      if problem.grid.Pattern.matrix.(y).(x) then
        incr realn
    done 
  done;
  !realn

let number_of_tile_columns problem =
  List.fold_left (
    fun (prim, sec as acc) e ->
      match e.Tile.multiplicity with
        | Mone -> (prim + 1, sec)
	| Mmaybe -> (prim, sec + 1)
        | Minf -> acc
  ) (0, 0) problem.pieces

(* return a boolean matrix representing the set of way to put all pieces
 * on the board
 * *)
let emc problem =
  let h = problem.grid.Pattern.height in 
  let w = problem.grid.Pattern.width in 
  let ncc = number_of_cell_columns problem in
  let prim, sec = number_of_tile_columns problem in
  let n = ncc + prim + sec in
  let tile_id_prim = ref ncc in
  let tile_id_sec  = ref (ncc + prim) in
  let lines = ref [] in 
  let add_piece i j tile = 
    let tile_id = match tile.multiplicity with
      | Mone -> let v = !tile_id_prim in incr tile_id_prim; v
      | Mmaybe -> let v = !tile_id_sec in incr tile_id_sec; v
      | Minf -> -1 (* useless *)
    in
    List.iter
      (fun t -> 
        if is_possible_position t problem i j then begin
          lines := one_line w n tile_id t problem i j :: !lines;
        end
      )
      (tile :: Tile.create_all_symetries tile)
  in 
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
        List.iter (add_piece i j) problem.pieces; 
        tile_id_prim := ncc;
        tile_id_sec := ncc + prim
    done 
  done;
  ncc + prim, Array.of_list !lines

 


