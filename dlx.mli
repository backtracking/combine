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

val print_first_solution : t -> unit

val get_first_solution : t -> int list
            
val count_solutions : t -> int

val get_solution_array : t -> int list array
  (** return all solutions *)

val get_solution_list : t -> int list list

val print_solution_array : int list array -> unit

val list_of_solution : node array * int -> int list 

val iter_solution : (node array * int -> unit) -> t -> unit
