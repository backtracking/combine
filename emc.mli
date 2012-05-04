
type problem = bool array array

type solution = int list
    (** a solution is a set of rows *)

module type S = sig

  val find_solution: problem -> solution
    (** raises [Not_found] if the problem has no solution *)

  val iter_solution: (solution -> unit) -> problem -> unit

  val count_solutions: problem -> int

  (*
  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end

  module Count(A: ARITH) : sig val count_solutions: problem -> A.t end
  *)
end

(** DLX-based implementation *)
module D: S

(** ZDD-based implementation *)
module Z: S

