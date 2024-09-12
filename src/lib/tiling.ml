(**************************************************************************)
(*                                                                        *)
(*  Combine - an OCaml library for combinatorics                          *)
(*                                                                        *)
(*  Copyright (C) 2012-2014                                               *)
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

(* Misc *)

module Pattern = struct

  type t = {
    matrix : bool array array;
    height : int;
    width  : int;
    size   : int;
  }

  let compute_size m =
    let s = ref 0 in Array.iter (Array.iter (fun b -> if b then incr s)) m; !s

  let create g =
    let h = Array.length g in
    if h = 0 then invalid_arg "Pattern.create";
    { matrix = g;
      height = h;
      width = Array.length g.(0);
      size = compute_size g }

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
    { matrix = new_m; height = new_h; width = new_w; size = p.size }

  let print fmt p =
    let even = ref 0 and odd = ref 0 in
    for y = p.height-1 downto 0 do
      Array.iteri (
        fun x cell ->
          if cell then Format.fprintf fmt "*"
          else Format.fprintf fmt ".";
          if cell then if (x + y) mod 2 = 0 then incr even else incr odd
      ) p.matrix.(y);
      if y > 0 then Format.fprintf fmt "@\n"
    done
    (* ; Format.fprintf fmt "(%d even, %d odd)@\n" !even !odd *)

  let resize p ~w ~h  =
    let min_w, min_h = min w p.width, min h p.height in
    let m = Array.make_matrix h w false in
    for y = 0 to min_h - 1 do
      for x = 0 to min_w - 1 do
        m.(y).(x) <- p.matrix.(y).(x)
      done
    done;
    { matrix = m; height = h; width = w; size = compute_size m }

  let crop p ~x ~y ~w ~h =
    let m = Array.make_matrix h w false in
    for y' = y to min p.height h - 1 do
      for x' = x to min p.width w - 1 do
        m.(y' - y).(x' - x) <- p.matrix.(y').(x')
      done
    done;
    {matrix = m; height = h; width = w; size = compute_size m }

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
    { matrix = m; height = h; width = w; size = compute_size m }

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
    { matrix = m; height = h; width = w; size = compute_size m }

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

type symmetries = Snone | Spositive | Sall
type multiplicity = Minf | Mone | Mmaybe

module Tile = struct

  type t = {
    name: string;
    pattern: Pattern.t;
    multiplicity : multiplicity;
    symmetries : symmetries;
    isos   : D4.subgroup;   (* the pattern is invariant by these isometries *)
  }

  let print fmt t =
    if t.name <> "" then Format.fprintf fmt "tile %S@\n" t.name;
    Format.fprintf fmt "%a@\n" Pattern.print t.pattern;
    Format.fprintf fmt "{ ";
    D4.S.iter (fun iso -> Format.fprintf fmt "%a, " D4.print iso)
      (D4.elements t.isos);
    Format.fprintf fmt "}"

  let create ?(name="") ?(s=Snone) ?(m=Minf) p =
    { name = name;
      pattern = p;
      multiplicity = m;
      symmetries = s;
      isos = D4.subgroup (Pattern.get_isos p); }

  let apply iso t =
    if D4.S.mem iso (D4.elements t.isos) then t
    else create ~m:t.multiplicity (Pattern.apply iso t.pattern)

(*
  let symmetries t =
    D4.S.fold
      (fun iso l -> apply iso t :: l )
      (D4.S.diff D4.all (D4.elements t.isos)) [t]
*)

  let create_all_symmetries t =
    let h = Hashtbl.create 8 in
    let l = match t.symmetries with
      | Snone -> D4.S.add D4.Id D4.S.empty
      | Spositive -> D4.S.filter D4.is_positive D4.all
      | Sall-> D4.all
    in
    D4.S.iter (fun iso -> Hashtbl.replace h (apply iso t) ())
      (D4.S.diff l (D4.elements t.isos));
    Hashtbl.fold (fun k _ acc -> k :: acc) h []

(*
  let create_all_symmetries t = match t.symmetries with
    | Snone ->
        [t]
    | Srotations | Sall as s ->
        let g = D4.elements (D4.quotient D4.d4 t.isos) in
	let g = if s = Srotations then D4.S.filter D4.is_positive g else g in
        List.map (fun iso -> apply iso t) (D4.S.elements g)

  let create_all_symmetries t =
    let l = create_all_symmetries t in
    Format.eprintf "@[<hov 2>create_all_symmetries:@\n";
    List.iter (fun t -> Format.eprintf "%a@\n" print t) l;
    Format.eprintf "@]";
    l
*)

end


module Problem = struct

  type problem = {
    grid : Pattern.t;
    pname : string;
    pieces : Tile.t list;
  }

  let create ?(name="") g ps = {
    grid = g;
    pname = name;
    pieces = ps;
  }

  let print fmt problem =
    if problem.pname <> "" then
      Format.fprintf fmt "problem %S@\n" problem.pname;
    Format.fprintf fmt "%a@\n" Pattern.print problem.grid;
    List.iter (fun t -> Format.fprintf fmt "%a@\n" Tile.print t) problem.pieces

  (** Solution *)

  open Pattern

  type solution = (Tile.t * int * int) list

  let put_char tile board x y c =
    for y' = 0 to tile.Tile.pattern.height - 1 do
      for x' = 0 to tile.Tile.pattern.width - 1 do
        if tile.Tile.pattern.matrix.(y').(x') then
          board.(y + y').(x' + x) <- c
      done
    done

  open Format

  let print_solution_ascii fmt p s =
    let unique = ref 33 in
    let board = Array.make_matrix p.grid.height p.grid.width '.' in
    List.iter (
      fun (t, x, y) ->
        put_char t board x y (Char.chr !unique);
        incr unique;
        if !unique = 46 then incr unique; (* skip '.' *)
        if !unique = 127 then Format.eprintf "too many tiles for ASCII output@."
    ) s;
    for y = p.grid.height - 1 downto 0 do
      for x = 0 to p.grid.width - 1 do
        fprintf fmt "%c" board.(y).(x)
      done;
      if y > 0 then fprintf fmt "@\n"
    done;
    fprintf fmt "@\n"

  let _print_board_svg width height u fmt =
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

  let print_solution_to_svg fmt ~width ~height p s =
    let u = width / p.grid.width in
    fprintf fmt
      "@[<hov 2><svg xmlns=\"http://www.w3.org/2000/svg\" \
width=\"%d\" height=\"%d\">@\n"
      width height;
  (* print_board_svg p.grid.width p.grid.height u fmt; *)
    let inc = golden_ratio *. 360. in
    Random.self_init ();
    let h = ref (Random.float 360.)  in
    List.iter (
      fun (t, x, y) ->
        let color = hsv_to_rgb !h 0.7 0.95 in
        h := !h +. inc;
        print_tile_svg p.grid.height x y u color fmt t;
    ) s;
    fprintf fmt "@]@\n</svg>"

  let print_solution_to_svg_file f ~width ~height p s =
    let c = open_out f in
    let fmt = formatter_of_out_channel c in
    print_solution_to_svg fmt ~width ~height p s ;
    fprintf fmt "@.";
    close_out c


(** Reduction to EMC *)

  module ToEMC = struct

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

    let one_line _n tile_id tile problem ~x ~y =
      let line = ref [] in
      for y' = 0 to tile.Tile.pattern.Pattern.height - 1 do
        for x' = 0 to tile.Tile.pattern.Pattern.width - 1 do
          if tile.Tile.pattern.Pattern.matrix.(y').(x') then begin
            line := get_id_col_emc problem (x + x') (y + y') :: !line;
            if tile.Tile.multiplicity <> Minf then
              line := tile_id :: !line
          end
        done
      done;
      List.sort Stdlib.compare !line

    let _one_line_piece_only n piece_id _piece =
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
      columns: int;
      primary: int;			      (* number of primary columns *)
      emc    : int list array;
      tiles  : (Tile.t * int * int) array;    (* row -> tile and its position *)
    }

    let print_emc fmt emc =
      let rec print_line i = function
        | _ when i = emc.columns -> ()
        | c :: l when c = i -> fprintf fmt "1"; print_line (i + 1) l
        | l -> fprintf fmt "0"; print_line (i + 1) l in
      Array.iter (print_line 0) emc.emc;
      fprintf fmt "%d primary columns" emc.primary

    let print_emc_size fmt emc =
      let h = Array.length emc.emc in
      fprintf fmt "%d rows x %d columns, with %d primary columns"
        h emc.columns emc.primary

  (* return a boolean matrix representing the set of way to put all pieces
   * on the board
   * *)
    let make problem =
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
              lines := one_line n tile_id t problem ~x ~y :: !lines;
              decodes :=  (t, x, y) :: !decodes
            end
          )
          (tile :: Tile.create_all_symmetries tile)
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
      { columns = n;
        primary = ncc + prim;
        emc = matrix;
        tiles = decode_tbl }

    let decode_solution d s = List.map (fun r -> d.(r)) s

let print_solution_to_svg fmt ~width ~height p {tiles=d; _} s =
      print_solution_to_svg fmt ~width ~height p (decode_solution d s)

let print_solution_to_svg_file f ~width ~height p {tiles=d; _} s =
      print_solution_to_svg_file f ~width ~height p (decode_solution d s)

let print_solution_ascii fmt p {tiles=d; _} s =
      print_solution_ascii fmt p (decode_solution d s)

  end

end

(** 3D tiling problems *)

module Tile3 = struct

  type t = {
    name: string;
    pattern: Pattern.t array;
    height: int;
    width: int;
    depth: int;
    multiplicity: multiplicity;
    symmetries: symmetries;
  }

  let create ?(name="") ?(s=Snone) ?(m=Minf) pl =
    let height, width = match pl with
      | [] -> invalid_arg "Tile3.create"
      | p :: _l -> p.Pattern.height, p.Pattern.width in
    let check p' = p'.Pattern.height = height && p'.Pattern.width = width in
    if not (List.for_all check pl) then
      invalid_arg "Tile3.create: sizes differ";
    { name = name;
      pattern = Array.of_list pl;
      height = height;
      width = width;
      depth = List.length pl;
      multiplicity = m;
      symmetries = s; }

  open Format

  let print fmt t =
    let print1 p = Pattern.print fmt p; fprintf fmt "@\n,@\n" in
    fprintf fmt "@[<hov 1>"; Array.iter print1 t.pattern; fprintf fmt "}@]"

  let apply iso p =
    let trans = Cube.apply iso in
    let w, h, d = p.width, p.height, p.depth in
    let new_w, new_h, new_d = Cube.trans_size iso (w, h, d) in
    let m = Array.init new_d (fun _ -> Array.make_matrix new_h new_w false) in
    for z = 0 to d-1 do
      for y = 0 to h-1 do
        for x = 0 to w-1 do
          let new_x, new_y, new_z = trans ~w ~h ~d (x, y, z)  in
          m.(new_z).(new_y).(new_x) <- p.pattern.(z).Pattern.matrix.(y).(x)
        done
      done
    done;
    let ml = Array.to_list m in
    let pl = List.map Pattern.create ml in
    create ~name:p.name ~s:p.symmetries ~m:p.multiplicity pl

  let create_all_symmetries t =
    match t.symmetries with
    | Snone -> [t]
    | Spositive | Sall ->
        let h = Hashtbl.create 8 in
        List.iter (fun iso -> Hashtbl.replace h (apply iso t) ())
          Cube.positive;
        Hashtbl.fold (fun k _ acc -> k :: acc) h []

end

module Problem3 = struct

  type problem = {
    grid : Tile3.t;
    pname : string;
    pieces : Tile3.t list;
  }

  let create ?(name="") grid pieces =
    { grid = Tile3.create ~name grid;
      pname = name;
      pieces = pieces; }

  open Format

  let print fmt p =
    if p.pname <> "" then
      Format.fprintf fmt "problem %S@\n" p.pname;
    Format.fprintf fmt "%a@\n" Tile3.print p.grid;
    List.iter (fun t -> Format.fprintf fmt "%a@\n" Tile3.print t) p.pieces

  type solution = (Tile3.t * int * int * int) list

  module ToEMC = struct

    type emc = {
      primary: int;			      (* number of primary columns *)
      emc    : bool array array;
      tiles  : (Tile3.t * int * int * int) array; (* row -> tile and position *)
    }

    let print_emc fmt emc =
      let print_bool b = if b then fprintf fmt "1" else fprintf fmt "0" in
      let print_line _ l = Array.iter print_bool l; fprintf fmt "@\n" in
      Array.iteri print_line emc.emc;
      fprintf fmt "%d primary columns" emc.primary

    let print_emc_size fmt emc =
      let h = Array.length emc.emc in
      fprintf fmt "%d rows x %d columns, with %d primary columns"
        h (if h = 0 then 0 else Array.length emc.emc.(0)) emc.primary

    open Pattern
    open Tile3

    let get_id_col_emc problem x y z =
      let id = ref 0 in
      try
        for z' = 0 to problem.grid.depth - 1 do
          for y' = 0 to problem.grid.height - 1 do
            for x' = 0 to problem.grid.width - 1 do
              if z' = z && y' = y && x' = x then raise Exit;
              if problem.grid.pattern.(z').matrix.(y').(x') then
                incr id
            done
          done
        done;
        !id
      with Exit -> !id

    let one_line n tile_id tile problem ~x ~y ~z =
      let line = Array.make n false in
      for z' = 0 to tile.depth - 1 do
        for y' = 0 to tile.height - 1 do
          for x' = 0 to tile.width - 1 do
            if tile.pattern.(z').matrix.(y').(x') then begin
              line.(get_id_col_emc problem (x + x') (y + y') (z + z')) <- true;
              if tile.Tile3.multiplicity <> Minf then
                line.(tile_id) <- true
            end
          done
        done
      done;
      line

    (* the total number of cells in the problem *)
    let number_of_cell_columns problem =
      let h = problem.grid.height in
      let w = problem.grid.width in
      let d = problem.grid.depth in
      let realn = ref 0 in
      for z = 0 to d - 1 do
        let m = problem.grid.pattern.(z).matrix in
        for y = 0 to h - 1 do
          for x = 0 to w - 1 do
            if m.(y).(x) then
              incr realn
          done
        done
      done;
      !realn

    let number_of_tile_columns problem =
      List.fold_left (
        fun (prim, sec as acc) e ->
          match e.Tile3.multiplicity with
            | Mone -> (prim + 1, sec)
	    | Mmaybe -> (prim, sec + 1)
            | Minf -> acc
      ) (0, 0) problem.pieces

    let existing_position problem x y z =
      z < problem.grid.depth &&
      let m = problem.grid.pattern.(z).Pattern.matrix in
      x < problem.grid.width
      && y < problem.grid.height
      && m.(y).(x)

  (* return true if piece could be put at position x y*)
    let is_possible_position tile board x y z =
      try
        for z' = 0 to tile.depth - 1 do
          let m = tile.pattern.(z').matrix in
          for y' = 0 to tile.height - 1 do
            for x' = 0 to tile.width - 1 do
              if m.(y').(x') &&
                 not (existing_position board (x + x') (y + y') (z + z'))
              then raise Exit
            done
          done
        done;
        true
      with
        | Exit ->  false

    let make problem =
      let h = problem.grid.height in
      let w = problem.grid.width in
      let d = problem.grid.depth in
      let ncc = number_of_cell_columns problem in
      let prim, sec = number_of_tile_columns problem in
      let n = ncc + prim + sec in
      let tile_id_prim = ref ncc in
      let tile_id_sec  = ref (ncc + prim) in
      let lines = ref [] in
      let decodes = ref [] in
      let add_piece x y z tile =
        let tile_id = match tile.multiplicity with
          | Mone -> let v = !tile_id_prim in incr tile_id_prim; v
          | Mmaybe -> let v = !tile_id_sec in incr tile_id_sec; v
          | Minf -> -1 (* useless *)
        in
        List.iter
          (fun t ->
            if is_possible_position t problem x y z then begin
              lines := one_line n tile_id t problem ~x ~y ~z :: !lines;
              decodes :=  (t, x, y, z) :: !decodes
            end
          )
          (tile :: Tile3.create_all_symmetries tile)
      in
      for z = 0 to d - 1 do
        for y = 0 to h - 1 do
          for x = 0 to w - 1 do
            List.iter (add_piece x y z) problem.pieces;
            tile_id_prim := ncc;
            tile_id_sec := ncc + prim
          done
        done
      done;
      let matrix = Array.of_list !lines in
      let decode_tbl = Array.of_list !decodes in
      { primary = ncc + prim;
        emc = matrix;
        tiles = decode_tbl }

    let print_solution_ascii fmt _p emc rows =
      let print r =
        let t, x, y, z = emc.tiles.(r) in
        fprintf fmt "tile '%s' at (%d, %d, %d)@\n" t.name x y z in
      List.iter print rows

  end

end
