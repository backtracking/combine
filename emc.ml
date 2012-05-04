(* EMC Module *)

type problem = bool array array

type solution = int list
    (** a solution is a set of rows *)

module type S = sig
  val find_solution: problem -> solution
  val iter_solution: (solution -> unit) -> problem -> unit
  val count_solutions: problem -> int
end

module D = struct
  let find_solution p = assert false (* TODO *)
  let iter_solution f p = assert false (* TODO *)
  let count_solutions p = assert false (* TODO *)
end

module Z = struct
  let find_solution p = assert false (* TODO *)
  let iter_solution f p = assert false (* TODO *)
  let count_solutions p = assert false (* TODO *)
end
