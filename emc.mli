
type problem = bool array array

type solution = int list
    (** a solution is a set of rows *)

module type S = sig

  val find_solution: problem -> solution
    (** raises [Not_found] if the problem has no solution *)

  val iter_solution: (solution -> unit) -> problem -> unit
    (** [Emc.iter_solution f p] applies [f] in turn to each problem [p] 
      solutions *)

  val simple_count_solutions: problem -> int
    (** Return the number of solutions to this problem *)

  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
    (** simple arithmetic module with big integers *)

  module Count(A: ARITH) : sig val count_solutions: problem -> A.t end
    (** Functor usage of arithmetics *)

end

val display_boolean_matrix : bool array array -> unit

  (** DLX-based implementation *)
module D: S

  (** ZDD-based implementation *)
module Z: S

