

module Iso : sig

  type t = Id | Rot90 | Rot180 | Rot270 | VertRefl | HorizRefl |
               Diag1Refl | Diag2Refl

  val compose: t -> t -> t
  
  val to_string : t -> string

  val apply : t -> w:int -> h:int -> int * int -> int * int
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

  val apply: Iso.t -> t -> t

  val resize: t -> w:int -> h:int -> t
  val crop  : t -> x:int -> y:int -> w:int -> h:int -> t

  val shift: t -> ofsx:int -> ofsy:int -> t

  val union: t -> t -> t
  val inter: t -> t -> t
  val diff : t -> t -> t
  val xor : t -> t -> t

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
  pname : string option;
  pieces : Tile.t list;
}

val create_problem : ?name:string -> Pattern.t -> Tile.t list -> problem

val print_problem: Format.formatter -> problem -> unit

val emc: problem -> int * bool array array


