
type unique = int
type zdd = private Bottom | Top | Node of unique * int * zdd * zdd

module S : Set.S with type elt = int
include Set.S with type t = zdd and type elt = S.t

val bottom: t
val top: t
val construct: int -> t -> t -> t

val choose_list: t -> int list
val iter_list: (int list -> unit) -> t -> unit

val unique : t -> int

module type ARITH = sig
  type t
  val zero: t
  val one: t
  val add: t -> t -> t
end
  
module Cardinal(A: ARITH) : sig val cardinal: t -> A.t end

val size: t -> int
  (** Number of internal nodes of a given ZDD.
      Each node occupies 5 words, so the total space used by a ZDD [z]
      is [size z * 5 * Sys.word_size lsl 3] bytes. *)

val print_to_dot: Format.formatter -> t -> unit
  (** [print_to_dot fmt z] write a string on dot format corresponding 
   to the drawing of the Zdd [z] *)

val print_to_dot_file: string -> t -> unit

