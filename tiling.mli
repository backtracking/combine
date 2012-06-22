

module Iso : sig

  type t = Id | Rot90 | Rot180 | Rot270 | VertRefl | HorizRefl |
               Diag1Refl | Diag2Refl

  val compose: t -> t -> t
    (* [compose i1 i2] returns the isometry result of the composition 
      of [i1] and [i2] *)
  
  val to_string : t -> string

  val apply : t -> w:int -> h:int -> int * int -> int * int
    (* [apply i w h x y] returns the couple result of the isometry [i]
       applied on the couple [x, y] *)

  val trans_size : t -> int * int -> int * int

  module S: Set.S with type elt = t

  val all: S.t

  val print : Format.formatter -> t -> unit

end

module Pattern : sig

  (*
    y ^
      |
      |
    0 +----------> x
      0
  *)

  type t = private {
    matrix   : bool array array;
    height : int;
    width  : int;
  }

  val create: bool array array -> t
    (* [create m] creates a pattern of type t from a boolean matrix*)

  val apply: Iso.t -> t -> t
    (* [apply i p] creates a new pattern which is the result of the 
       transformation from [p] by [i] *)

  val resize: t -> w:int -> h:int -> t
    (* [resize p w h] change the size of [p] to [w] (width) [h] (height) 
      elements which are over [w] and [h] will not appears *)


  val crop  : t -> x:int -> y:int -> w:int -> h:int -> t
    (* [crop p x y w h] creates a pattern from [p] with a rectangle of size
       [w, h] drawned in position [x, y] on [p] *)

  val shift: t -> ofsx:int -> ofsy:int -> t
    (* [shift p ofsx ofsy] creates a pattern from the shifting [osfx, osfy]
      of [p] *)

  val union: t -> t -> t
    (* [union p1 p2] creates a pattern from the logical union beetween [p1] and 
        [p2] *)

  val inter: t -> t -> t
    (* [inter p1 p2] creates a pattern from the logical intersection 
     beetween [p1] and  [p2] *)

  val diff : t -> t -> t
    (* [inter p1 p2] creates a pattern from the logical difference 
     beetween [p1] and  [p2] *)

  val xor : t -> t -> t
    (* [xor p1 p2] creates a pattern from the logical xor 
     beetween [p1] and  [p2] *)

  val has_iso: Iso.t -> t -> bool

  val print : Format.formatter -> t -> unit

end 

module Tile : sig

  type symetries = Snone | Srotations | Sall
  type multiplicity = Minf | Mone | Mmaybe

  type t = private {
    name: string option;
    pattern: Pattern.t;
    multiplicity : multiplicity;
    symetries : symetries;
    isos   : Iso.S.t;   (* the pattern is invariant by these isometries *)
  }

  val create: 
             ?name:string -> ?s:symetries -> ?m:multiplicity -> Pattern.t -> t

  val apply: Iso.t -> t -> t

  val print : Format.formatter -> t -> unit

end

type problem = private {
  grid : Pattern.t;
  pname : string;
  pieces : Tile.t list;
}

val create_problem : ?name:string -> Pattern.t -> Tile.t list -> problem

val print_problem: Format.formatter -> problem -> unit

val emc: 
  problem -> int * bool array array * (int, Tile.t * int * int) Hashtbl.t


val print_solution_to_svg : 
int -> int -> problem -> ('a, Tile.t * int * int) Hashtbl.t ->
  Format.formatter -> 'a list -> unit


val print_solution_to_svg_file : 
string -> 'a list -> problem -> ('a, Tile.t * int * int) Hashtbl.t ->
  int -> int -> unit


val print_solution_ascii : problem -> ('a, Tile.t * int * int) Hashtbl.t ->
  Format.formatter -> 'a list -> unit




