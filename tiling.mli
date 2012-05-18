

module Iso : sig

  type t = Id | Rot90 | Rot180 | Rot270 | VertRefl | HorizRefl |
               Diag1Refl | Diag2Refl

  val compose: t -> t -> t
  
  val to_string : t -> string

  val apply : t -> ?w:int -> ?h:int -> int * int -> int * int
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
    grid   : bool array array;
    height : int;
    width  : int;
  }

  val create: bool array array -> t

  val apply: Iso.t -> t -> t

(***
  val resize: t -> w:int -> h:int -> t
  val crop  : t -> x:int -> y:int -> w:int -> h:int -> t

  val shift: t -> ofsx:int -> ofsy:int -> t

  val union: t -> t -> t
  val inter: t -> t -> t
  val diff : t -> t -> t
***)

  val has_iso: Iso.t -> t -> bool

  val print : Format.formatter -> t -> unit

end 

module Tile : sig

  type t = private {
    pattern: Pattern.t;
    isos   : Iso.S.t;   (* the pattern is invariant by these isometries *)
  }

  val create: Pattern.t -> t

  val apply: Iso.t -> t -> t

  val print : Format.formatter -> t -> unit

end

(***
val dominos: Tile.t list
val pentaminos: Tile.t list
***)

type piece

val create_piece : ?q:int -> ?e:bool -> ?n:string -> bool array array -> piece

type problem

val create_problem : ?n:string -> bool array array -> piece list -> problem

val emc: problem -> bool array array


