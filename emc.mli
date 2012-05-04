
type problem = bool array array

type solution = int list
    (** a solution is a set of rows *)

module type S = sig

  val find_solution: problem -> solution
    (** raises [Not_found] if the problem has no solution *)

  val iter_solution: (solution -> unit) -> problem -> unit

  val count_solution: problem -> int

end

