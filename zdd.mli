
type unique = int
type t = private Bottom | Top | Node of unique * int * t * t

val bottom: t
val top: t

val construct: int -> t -> t -> t

val cardinal: t -> int
val unique : t -> int

val inter: t -> t -> t

type set = int list

             (*
module S : Set.S with type elt = int
*)

val any_element: t -> set
  (** [any_element z] find and return a solution in an arbitrary way 
  Not_found is raised if there is no solution *)



val iter_element: (set -> unit) -> t -> unit



val size: t -> int
  (** Number of internal nodes of a given ZDD.
      Each node occupies 5 words, so the total space used by a ZDD [z]
      is [size z * 5 * Sys.word_size lsl 3] bytes. *)

val print_to_dot: Format.formatter -> t -> unit
  (** [print_to_dot fmt z] write a string on dot format corresponding 
   to the drawing of the Zdd [z] *)


(* à déplacer *)
val column: int -> bool array array -> t
val tiling: bool array array -> t
