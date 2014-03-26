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

(** Tiling problems modeling and encoding to EMC *)

(** Pattern representation as boolean matrix *)
module Pattern : sig

  (*
    y ^
      |
      |
    0 +----------> x
      0
  *)

  type t = private {
    matrix : bool array array;
    height : int;
    width  : int;
    size   : int;
  }

  val create: bool array array -> t
    (** [create m] creates a pattern of type t from a boolean matrix*)

  val apply: D4.t -> t -> t
    (** [apply i p] creates a new pattern which is the result of the
       transformation of [p] by [i] *)

  val resize: t -> w:int -> h:int -> t
    (** [resize p w h] change the size of [p] to [w] (width) and [h] (height).
      Points which are over [w] and [h] will not appear. *)

  val crop  : t -> x:int -> y:int -> w:int -> h:int -> t
    (** [crop p x y w h] creates a pattern from [p] with the rectangle of size
       [w, h] at position [x, y] in [p] *)

  val shift: t -> ofsx:int -> ofsy:int -> t
    (** [shift p ofsx ofsy] shift pattern [p] by [osfx, osfy] *)

  val union: t -> t -> t
    (** [union p1 p2] creates a pattern from the logical union beetween [p1] and
        [p2] *)

  val inter: t -> t -> t
    (** [inter p1 p2] creates a pattern from the logical intersection
     beetween [p1] and  [p2] *)

  val diff : t -> t -> t
    (** [inter p1 p2] creates a pattern from the logical difference
     beetween [p1] and  [p2] *)

  val xor : t -> t -> t
    (** [xor p1 p2] creates a pattern from the logical xor
     beetween [p1] and  [p2] *)

  val has_iso: D4.t -> t -> bool
    (** [has_iso t p] returns true if [p] is invariant by [i] *)

  val print : Format.formatter -> t -> unit
    (** print a pattern *)

end

module Tile : sig

  type symetries = Snone | Srotations | Sall
  type multiplicity = Minf | Mone | Mmaybe

  type t = private {
    name: string option;
    pattern: Pattern.t;
    multiplicity: multiplicity;
    symetries: symetries;
    isos: D4.subgroup;   (* the pattern is invariant by these isometries *)
  }

  val create: ?name:string -> ?s:symetries -> ?m:multiplicity -> Pattern.t -> t
    (** construct a tile from his name, its symetries [s] and
        its multiplicity [m]. [s] defaults to [Snone] and [m] defaults
        to [Minf] *)

  val apply: D4.t -> t -> t
    (** [apply i t] creates a new tile by applying transformation [i] to [t] *)

  val print: Format.formatter -> t -> unit
    (** print a tile *)

end

type problem = private {
  grid : Pattern.t;
  pname : string;
  pieces : Tile.t list;
}

val create_problem : ?name:string -> Pattern.t -> Tile.t list -> problem
  (** construct a problem from his name, the board pattern and
      a list of tiles *)

val print_problem: Format.formatter -> problem -> unit
  (** print a problem *)

type solution = (Tile.t * int * int) list

val print_solution_to_svg : Format.formatter ->
  width:int -> height:int -> problem -> solution -> unit
  (** print a solution under the svg format *)

val print_solution_to_svg_file : string ->
  width:int -> height:int -> problem -> solution -> unit
  (** print a solution to the svg format on the given file *)

val print_solution_ascii :
  Format.formatter -> problem -> solution -> unit
  (** print a solution with ascii symboles to draw tiles*)


module ToEMC: sig

  type emc = {
    primary: int;			      (* number of primary columns *)
    matrix : bool array array;
    tiles  : (Tile.t * int * int) array;      (* row -> tile and its position *)
  }

  val print_emc: Format.formatter -> emc -> unit
  val print_emc_size: Format.formatter -> emc -> unit

  val make: problem -> emc
    (** Encode the given problem under EMC *)

  val print_solution_to_svg : Format.formatter ->
    width:int -> height:int -> problem -> emc -> int list -> unit
    (** print a solution under the svg format *)

  val print_solution_to_svg_file : string ->
    width:int -> height:int -> problem -> emc -> int list -> unit
    (** print a solution to the svg format on the given file *)

  val print_solution_ascii :
    Format.formatter -> problem -> emc -> int list -> unit
    (** print a solution with ascii symboles to draw tiles*)

end




