(**************************************************************************)
(*                                                                        *)
(*  Combine - an OCaml library for combinatorics                          *)
(*                                                                        *)
(*  Copyright (C) 2012                                                    *)
(*    Remy El Sibaie                                                      *)
(*    Jean-Christophe Filliatre                                           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Tiling Module *)

open Format

(* Misc *)

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
    let trans = D4.apply iso in
    let w, h = p.width, p.height in
    let new_w, new_h = D4.trans_size iso (w, h) in
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
      invalid_arg "binary_op";
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

  let has_iso iso p =
    let w = p.width and h = p.height in
    let new_h, new_w = D4.trans_size iso (h, w) in
    new_w = p.width && new_h = p.height &&
    let trans = D4.apply iso in
    try
      for y = 0 to p.height - 1 do
        for x = 0 to p.width -1 do
          let new_x, new_y = trans ~w ~h (x, y) in
          if p.matrix.(new_y).(new_x) <> p.matrix.(y).(x) then raise Exit
        done
      done;
      true
    with Exit ->
      false

  let get_isos p =
    D4.S.filter (fun iso -> has_iso iso p) D4.all

end

module Tile = struct

  type symetries = Snone | Srotations | Sall
  type multiplicity = Minf | Mone | Mmaybe

  type t = {
    name: string option;
    pattern: Pattern.t;
    multiplicity : multiplicity;
    symetries : symetries;
    isos   : D4.subgroup;   (* the pattern is invariant by these isometries *)
  }

  let print fmt t =
    begin match t.name with Some s ->
    Format.printf "tile %S@\n"s; | None -> () end;
    Format.fprintf fmt "%a@\n" Pattern.print t.pattern;
    Format.fprintf fmt "{ ";
    D4.S.iter (fun iso -> Format.fprintf fmt "%a, " D4.print iso)
      (D4.elements t.isos);
    Format.fprintf fmt "}"

  let create ?name ?(s=Snone) ?(m=Minf) p =
    { name = name;
      pattern = p;
      multiplicity = m;
      symetries = s;
      isos = D4.subgroup (Pattern.get_isos p); }

  let apply iso t =
    if D4.S.mem iso (D4.elements t.isos) then t
    else create ~m:t.multiplicity (Pattern.apply iso t.pattern)

(*
  let symetries t =
    D4.S.fold
      (fun iso l -> apply iso t :: l )
      (D4.S.diff D4.all (D4.elements t.isos)) [t]
*)

  let create_all_symetries t =
    let h = Hashtbl.create 8 in
    let l = match t.symetries with
      | Snone -> D4.S.add D4.Id D4.S.empty
      | Srotations -> D4.S.filter D4.is_positive D4.all
      | Sall-> D4.all
    in
    D4.S.iter (fun iso -> Hashtbl.replace h (apply iso t) ())
      (D4.S.diff l (D4.elements t.isos));
    Hashtbl.fold (fun k _ acc -> k :: acc) h []

(*
  let create_all_symetries t = match t.symetries with
    | Snone ->
        [t]
    | Srotations | Sall as s ->
        let g = D4.elements (D4.quotient D4.d4 t.isos) in
	let g = if s = Srotations then D4.S.filter D4.is_positive g else g in
        List.map (fun iso -> apply iso t) (D4.S.elements g)

  let create_all_symetries t =
    let l = create_all_symetries t in
    Format.eprintf "@[<hov 2>create_all_symetries:@\n";
    List.iter (fun t -> Format.eprintf "%a@\n" print t) l;
    Format.eprintf "@]";
    l
*)

end

type problem = {
  grid : Pattern.t;
  pname : string;
  pieces : Tile.t list;
}

let create_problem ?(name="") g ps = {
  grid = g;
  pname = name;
  pieces = ps;
}

let print_problem fmt problem =
  if problem.pname <> "" then Format.fprintf fmt "problem %S@\n" problem.pname;
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

let one_line l n tile_id tile problem ~x ~y =
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

type emc = {
  primary: int;			        (* number of primary columns *)
  matrix : bool array array;
  tiles  : (Tile.t * int * int) array;	(* row -> tile and its position *)
}

let print_emc fmt emc =
  let print_bool b = if b then fprintf fmt "1" else fprintf fmt "0" in
  let print_line _ l = Array.iter print_bool l; fprintf fmt "@\n" in
  Array.iteri print_line emc.matrix;
  fprintf fmt "%d primary columns" emc.primary

let print_emc_size fmt emc =
  let h = Array.length emc.matrix in
  fprintf fmt "%d rows x %d columns, with %d primary columns"
    h (if h = 0 then 0 else Array.length emc.matrix.(0)) emc.primary

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
  let decodes = ref [] in
  let add_piece x y tile =
    let tile_id = match tile.multiplicity with
      | Mone -> let v = !tile_id_prim in incr tile_id_prim; v
      | Mmaybe -> let v = !tile_id_sec in incr tile_id_sec; v
      | Minf -> -1 (* useless *)
    in
    List.iter
      (fun t ->
        if is_possible_position t problem x y then begin
           lines := one_line w n tile_id t problem x y :: !lines;
           decodes :=  (t, x, y) :: !decodes
         end
      )
      (tile :: Tile.create_all_symetries tile)
  in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      List.iter (add_piece x y) problem.pieces;
      tile_id_prim := ncc;
      tile_id_sec := ncc + prim
    done
  done;
  let matrix = Array.of_list !lines in
  let decode_tbl = Array.of_list !decodes in
  { primary = ncc + prim;
    matrix = matrix;
    tiles = decode_tbl }

open Format
open Pattern
open Char
open Graphics

let print_board_svg width height u fmt =
  for i = 0 to height do
    fprintf fmt "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" \
style=\"stroke:black;stroke-width:1;\" />@\n"
      0 (i * u) (u * width) (i * u)
  done;
  for i = 0 to width do
    fprintf fmt "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" \
style=\"stroke:black;stroke-width:1;\" />@\n"
      (i * u) 0 (i * u) (u * height)
  done

let print_square_svg x y u color fmt =
  let r, g, b = color in
  fprintf fmt
    "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" \
    style=\"fill:rgb(%d, %d, %d);\" />@\n"
    (x * u) (y * u) u u r g b

let print_tile_svg height x y u color fmt t =
  fprintf fmt
    "<g style=\"stroke:#000000;stroke-width:1;\
stroke-linejoin:miter;stroke-miterlimit:4;stroke-opacity:1;\">@\n";
  for y' = 0 to t.Tile.pattern.height - 1 do
    for x' = 0 to t.Tile.pattern.width - 1 do
      if t.Tile.pattern.matrix.(y').(x') then
        print_square_svg (x + x') (height - 1 - (y + y')) u color fmt
    done
  done;
  fprintf fmt "</g>@\n"

let golden_ratio = 0.618033988749895

let hsv_to_rgb h s v =
    let c = v *. s in
    let h = int_of_float h in
    let hh = (h mod 360)/60 in
    let hhf = (mod_float (float_of_int h) 360.) /. 60. in
    let x = c *. (1. -. (abs_float (mod_float hhf 2. -. 1.))) in
    let m = v -. c in
    let cc = int_of_float ((c +. m) *. 255.) in
    let xx = int_of_float ((x +. m) *. 255.) in
    let mm = int_of_float (m *. 255.) in
    match hh with
    | 0 -> cc, xx, mm
    | 1 -> xx, cc, mm
    | 2 -> mm, cc, xx
    | 3 -> mm, xx, cc
    | 4 -> xx, mm, cc
    | 5 -> cc, mm, xx
    | _ -> mm, mm, mm

let print_solution_to_svg fmt ~width ~height p {tiles=decoder} s =
  let u = width / p.grid.width in
  fprintf fmt
"<?xml version=\"1.0\" standalone=\"no\"?> @\n\
@[<hov 2><svg xmlns=\"http://www.w3.org/2000/svg\" \
width=\"%d\" height=\"%d\">@\n"
  width height;
  (* print_board_svg p.grid.width p.grid.height u fmt; *)
  let inc = golden_ratio *. 360. in
  Random.self_init ();
  let h = ref (Random.float 360.)  in
  List.iter (
    fun e ->
      let color = hsv_to_rgb !h 0.7 0.95 in
      h := !h +. inc;
      let t, x, y = decoder.(e) in
      print_tile_svg p.grid.height x y u color fmt t;
  ) s;
  fprintf fmt "@]@\n</svg>"

let print_solution_to_svg_file f ~width ~height p emc s =
  let c = open_out f in
  let fmt = formatter_of_out_channel c in
  print_solution_to_svg fmt ~width ~height p emc s ;
  fprintf fmt "@.";
  close_out c

let put_char tile board x y c =
  for y' = 0 to tile.Tile.pattern.height - 1 do
    for x' = 0 to tile.Tile.pattern.width - 1 do
      if tile.Tile.pattern.matrix.(y').(x') then
        board.(y + y').(x' + x) <- c
    done
  done

let print_solution_ascii fmt p {tiles=d} s =
  let unique = ref 48 in
  let board = Array.make_matrix p.grid.height p.grid.width '.' in
  List.iter (
    fun e ->
      let t, x, y = d.(e) in
      put_char t board x y (chr !unique);
      incr unique
  ) s;
  for y = p.grid.height - 1 downto 0 do
    for x = 0 to p.grid.width - 1 do
      fprintf fmt "%c" board.(y).(x)
    done;
    if y > 0 then fprintf fmt "@\n"
  done;
  fprintf fmt "@\n"
