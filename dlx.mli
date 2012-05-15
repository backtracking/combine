(* Dlx Module *)

type node = {
    mutable c : node;
    mutable s: int;
    mutable up: node;
    mutable down: node;
    mutable left: node;
    mutable right: node;
    mutable name : string;
}

type t

val create: ?primary:int -> bool array array -> t
  (** If [primary] is given, it means that the first [primary] columns are
      primary; otherwise, all columns are primary columns *)

val print_solutions: t -> unit
  (* Print all solutions *)

val print_first_solution : t -> unit
  (** print the first arbitrary founded solution *)

val get_first_solution : t -> int list
  (** return the number of solutions of the given problem *)
            
val count_solutions : t -> int
  (** return the number of solutions of the given problem *)

val get_solution_array : t -> int list array
  (** return all solutions as an array *)

val get_solution_list : t -> int list list
  (** return all solutions as a list *)

val print_solutions_array : int list array -> unit
  (** Print all solutions *)

val list_of_solution : node array * int -> int list 
  (** return a list from a given solution *)

val iter_solution : (node array * int -> unit) -> t -> unit
  (** [inter_solution f p] applies [f] on all solutions of the problem [p]*)
