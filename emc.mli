
type solution = int list
    (** a solution is a set of rows *)

module type S = sig

  type t

  val create: ?primary:int -> bool array array -> t

  val find_solution: t -> solution
    (** raises [Not_found] if the problem has no solution *)

  val iter_solution: (solution -> unit) -> t -> unit
    (** [Emc.iter_solution f p] applies [f] in turn to each problem [p] 
      solutions *)

  val count_solutions: t -> int
    (** Return the number of solutions to this problem *)

  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
    (** simple arithmetic module with big integers *)

  module Count(A: ARITH) : sig val count_solutions: t -> A.t end
    (** Functor usage of arithmetics *)

end

val print_boolean_matrix : bool array array -> unit
val print_problem_size: bool array array -> unit


  (** DLX-based implementation *)
module D: S

  (** ZDD-based implementation *)
module Z: S with type t = Zdd.t


