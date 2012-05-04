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
  let find_solution p = Dlx.get_first_solution p
  let iter_solution f p = List.iter f (Dlx.get_solution_list p)
  let count_solutions = Dlx.count_solutions 
end

module Z = struct
  let find_solution p = Zdd.any_element (Zdd.tiling p)
  let iter_solution f p = Zdd.iter_element f (Zdd.tiling p)
  let count_solutions p = Zdd.cardinal (Zdd.tiling p)
end
